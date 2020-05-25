package org.clulab.dynet

import java.io.PrintWriter

import edu.cmu.dynet.{Dim, Expression, ExpressionVector, Parameter, ParameterCollection}
import org.clulab.dynet.ForwardLayer.{DROPOUT_PROB, TYPE_GREEDY}
import org.clulab.dynet.Utils.{ByLineIntBuilder, ByLineStringMapBuilder, fromIndexToString, save}

import ForwardLayer._

class GreedyForwardLayer (parameters:ParameterCollection,
                          inputSize: Int,
                          hasPredicate: Boolean,
                          t2i: Map[String, Int],
                          i2t: Array[String],
                          H: Parameter,
                          dropoutProb: Float = DROPOUT_PROB)
  extends ForwardLayer(parameters, inputSize, hasPredicate, t2i, i2t, H, dropoutProb) {

  override def loss(finalStates: ExpressionVector, goldLabelStrings: IndexedSeq[String]): Expression = {
    val goldLabels = Utils.toIds(goldLabelStrings, t2i)
    Utils.sentenceLossGreedy(finalStates, goldLabels)
  }

  override def saveX2i(printWriter: PrintWriter): Unit = {
    save(printWriter, TYPE_GREEDY, "inferenceType")
    save(printWriter, inputSize, "inputSize")
    save(printWriter, if(hasPredicate) 1 else 0, "hasPredicate")
    save(printWriter, t2i, "t2i")
  }

  override def toString: String = {
    s"GreedyForwardLayer($inDim, $outDim)"
  }

  override def inference(emissionScores: Array[Array[Float]]): IndexedSeq[String] = {
    val labelIds = Utils.greedyPredict(emissionScores)
    labelIds.map(i2t(_))
  }
}

object GreedyForwardLayer {
  def load(parameters: ParameterCollection,
           x2iIterator: BufferedIterator[String]): GreedyForwardLayer = {
    //
    // load the x2i info
    //
    val byLineIntBuilder = new ByLineIntBuilder()
    val byLineStringMapBuilder = new ByLineStringMapBuilder()

    val inputSize = byLineIntBuilder.build(x2iIterator, "inputSize")
    val hasPredicateAsInt = byLineIntBuilder.build(x2iIterator, "hasPredicate", DEFAULT_HAS_PREDICATE)
    val hasPredicate = hasPredicateAsInt == 1
    val t2i = byLineStringMapBuilder.build(x2iIterator, "t2i")
    val i2t = fromIndexToString(t2i)

    //
    // make the loadable parameters
    //
    println(s"making FF ${t2i.size} x ${2 * inputSize}")
    val actualInputSize = if(hasPredicate) 2 * inputSize else inputSize
    val H = parameters.addParameters(Dim(t2i.size, actualInputSize))

    new GreedyForwardLayer(parameters,
      inputSize, hasPredicate, t2i, i2t, H)
  }
}


