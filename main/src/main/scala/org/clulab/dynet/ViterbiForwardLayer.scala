package org.clulab.dynet
import java.io.PrintWriter

import edu.cmu.dynet.Expression.{lookup, randomNormal}
import edu.cmu.dynet.{Dim, Expression, ExpressionVector, FloatVector, LookupParameter, Parameter, ParameterCollection}
import org.clulab.dynet.Utils.{ByLineIntBuilder, ByLineStringMapBuilder, LOG_MIN_VALUE, START_TAG, STOP_TAG, fromIndexToString, mkTransitionMatrix, save}
import ForwardLayer._

class ViterbiForwardLayer(parameters:ParameterCollection,
                          inputSize: Int,
                          hasPredicate: Boolean,
                          t2i: Map[String, Int],
                          i2t: Array[String],
                          H: Parameter,
                          val T: LookupParameter, // transition matrix for Viterbi; T[i][j] = transition *to* i *from* j, one per task
                          dropoutProb: Float = DROPOUT_PROB)
  extends ForwardLayer(parameters, inputSize, hasPredicate, t2i, i2t, H, dropoutProb) {

  // call this *before* training a model, but not on a saved model
  def initializeTransitions(): Unit = {
    val startTag = t2i(START_TAG)
    val stopTag = t2i(STOP_TAG)

    for (i <- 0 until t2i.size) {
      T.initialize(i, initTransitionsTo(i, t2i.size, startTag, stopTag))
    }
  }

  private def initTransitionsTo(dst: Int, size:Int, startTag: Int, stopTag: Int): FloatVector = {
    val transScores = new Array[Float](size)

    for(i <- 0 until size) {
      transScores(i) = randomNormal(Dim(1)).value().toFloat() / size // pseudo Glorot
    }

    // discourage transitions to START from anything
    if (dst == startTag) {
      for (i <- 0 until size)
        transScores(i) = LOG_MIN_VALUE
    } else {
      // discourage transitions to anything from STOP
      transScores(stopTag) = LOG_MIN_VALUE

      // discourage transitions to I-X from B-Y or I-Y
      val dstTag = i2t(dst)
      if (dstTag.startsWith("I-")) {
        for (i <- 0 until size) {
          val srcTag = i2t(i)
          if ((srcTag.startsWith("B-") || srcTag.startsWith("I-")) &&
            srcTag.substring(2) != dstTag.substring(2)) {
            transScores(i) = LOG_MIN_VALUE
          }
        }
      }
    }

    new FloatVector(transScores)
  }

  override def loss(finalStates: ExpressionVector, goldLabelStrings: IndexedSeq[String]): Expression = {
    // fetch the transition probabilities from the lookup storage
    val transitionMatrix = new ExpressionVector
    for(i <- 0 until t2i.size) {
      transitionMatrix.add(lookup(T, i))
    }

    val goldLabels = Utils.toIds(goldLabelStrings, t2i)
    Utils.sentenceLossCrf(finalStates, transitionMatrix, goldLabels, t2i)
  }

  override def saveX2i(printWriter: PrintWriter): Unit = {
    save(printWriter, TYPE_VITERBI, "inferenceType")
    save(printWriter, inputSize, "inputSize")
    save(printWriter, if(hasPredicate) 1 else 0, "hasPredicate")
    save(printWriter, t2i, "t2i")
  }

  override def toString: String = {
    s"ViterbiForwardLayer($inDim, $outDim)"
  }

  override def inference(emissionScores: Array[Array[Float]]): IndexedSeq[String] = {
    val transitionMatrix: Array[Array[Float]] =
      Utils.transitionMatrixToArrays(T, t2i.size)
    val labelsIds = Utils.viterbi(emissionScores,
      transitionMatrix, t2i.size, t2i(START_TAG), t2i(STOP_TAG))
    labelsIds.map(i2t(_))
  }
}

object ViterbiForwardLayer {
  def load(parameters: ParameterCollection,
           x2iIterator: BufferedIterator[String]): ViterbiForwardLayer = {
    //
    // load the x2i info
    //
    val byLineIntBuilder = new ByLineIntBuilder()
    val byLineStringMapBuilder = new ByLineStringMapBuilder()

    val inputSize = byLineIntBuilder.build(x2iIterator)
    val hasPredicateAsInt = byLineIntBuilder.build(x2iIterator, "hasPredicate", DEFAULT_HAS_PREDICATE)
    val hasPredicate = hasPredicateAsInt == 1
    val t2i = byLineStringMapBuilder.build(x2iIterator)
    val i2t = fromIndexToString(t2i)

    //
    // make the loadable parameters
    //
    val actualInputSize = if(hasPredicate) 2 * inputSize else inputSize
    val H = parameters.addParameters(Dim(t2i.size, actualInputSize))
    val T = mkTransitionMatrix(parameters, t2i)

    new ViterbiForwardLayer(parameters,
      inputSize, hasPredicate, t2i, i2t, H, T)
  }
}

