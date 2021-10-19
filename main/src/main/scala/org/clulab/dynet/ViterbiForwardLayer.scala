package org.clulab.dynet
import java.io.PrintWriter
import edu.cmu.dynet.Expression.{lookup, randomNormal}
import edu.cmu.dynet.{Dim, Expression, ExpressionVector, FloatVector, LookupParameter, Parameter, ParameterCollection}
import org.clulab.dynet.Utils.{ByLineFloatBuilder, ByLineIntBuilder, ByLineStringBuilder, ByLineStringMapBuilder, LOG_MIN_VALUE, START_TAG, STOP_TAG, fromIndexToString, mkTransitionMatrix, save}
import ForwardLayer._

class ViterbiForwardLayer(parameters:ParameterCollection,
                          inputSize: Int,
                          isDual: Boolean,
                          t2i: Map[String, Int],
                          i2t: Array[String],
                          H: Parameter,
                          val T: LookupParameter, // transition matrix for Viterbi; T[i][j] = transition *to* i *from* j, one per task
                          rootParam: Parameter,
                          span: Option[Seq[(Int, Int)]],
                          nonlinearity: Int,
                          dropoutProb: Float)
  extends ForwardLayer(parameters, inputSize, isDual, t2i, i2t, H, rootParam, span, nonlinearity, dropoutProb) {

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
    save(printWriter, if (isDual) 1 else 0, "isDual")
    save(printWriter, span.map(spanToString).getOrElse("none"), "span")
    save(printWriter, nonlinearity, "nonlinearity")
    save(printWriter, t2i, "t2i")
    save(printWriter, dropoutProb, "dropoutProb")
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

  override def inferenceWithScores(emissionScores: Array[Array[Float]]): IndexedSeq[IndexedSeq[(String, Float)]] =
    throw new RuntimeException("ERROR: inferenceWithScores not supported for ViterbiLayer!")
}

object ViterbiForwardLayer {
  def load(parameters: ParameterCollection,
           x2iIterator: BufferedIterator[String]): ViterbiForwardLayer = {
    //
    // load the x2i info
    //
    val byLineIntBuilder = new ByLineIntBuilder()
    val byLineFloatBuilder = new ByLineFloatBuilder()
    val byLineStringMapBuilder = new ByLineStringMapBuilder()
    val byLineStringBuilder = new ByLineStringBuilder()

    val inputSize = byLineIntBuilder.build(x2iIterator)
    val isDualAsInt = byLineIntBuilder.build(x2iIterator, "isDual", DEFAULT_IS_DUAL)
    val isDual = isDualAsInt == 1
    val spanValue = byLineStringBuilder.build(x2iIterator, "span", "")
    val span = if(spanValue.isEmpty || spanValue == "none") None else Some(parseSpan(spanValue, inputSize))
    val nonlinearity = byLineIntBuilder.build(x2iIterator, "nonlinearity", ForwardLayer.NONLIN_NONE)
    val t2i = byLineStringMapBuilder.build(x2iIterator)
    val i2t = fromIndexToString(t2i)
    val dropoutProb = byLineFloatBuilder.build(x2iIterator, "dropoutProb", ForwardLayer.DEFAULT_DROPOUT_PROB)

    //
    // make the loadable parameters
    //
    //val actualInputSize = if(isDual) 2 * inputSize else inputSize
    val actualInputSize =
    if(span.nonEmpty) {
      val len = ForwardLayer.spanLength(span.get)
      if(isDual) 2 * len else len
    } else {
      if(isDual) 2 * inputSize else inputSize
    }
    val H = parameters.addParameters(Dim(t2i.size, actualInputSize))
    val rootParam = parameters.addParameters(Dim(inputSize))
    val T = mkTransitionMatrix(parameters, t2i)

    new ViterbiForwardLayer(parameters,
      inputSize, isDual, t2i, i2t, H, T, rootParam,
      span, nonlinearity, dropoutProb)
  }
}

