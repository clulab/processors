package org.clulab.dynet

import java.io.PrintWriter

import edu.cmu.dynet.{Dim, Expression, ExpressionVector, Parameter, ParameterCollection}
import org.clulab.dynet.ForwardLayer.{DROPOUT_PROB, TYPE_GREEDY}
import org.clulab.dynet.Utils.{ByLineIntBuilder, ByLineStringMapBuilder, fromIndexToString, save}

class GreedyForwardLayer (parameters:ParameterCollection,
                          inputSize: Int,
                          t2i: Map[String, Int],
                          i2t: Array[String],
                          H: Parameter,
                          dropoutProb: Float = DROPOUT_PROB)
  extends ForwardLayer(parameters, inputSize, t2i, i2t, H, dropoutProb) {

  override def loss(finalStates: ExpressionVector, goldLabels: IndexedSeq[Int]): Expression = {
    Utils.sentenceLossGreedy(finalStates, goldLabels)
  }

  override def forward(inputExpressions: ExpressionVector,
                       predicatePositionOpt: Option[Int],
                       doDropout: Boolean): ExpressionVector = {
    val pH = Expression.parameter(H)
    val emissionScores = new ExpressionVector()

    for(ie <- inputExpressions) {

      // TODO: dropout on ie here?

      var l1 = pH * ie
      if(doDropout) {
        l1 = Expression.dropout(l1, dropoutProb)
      }
      emissionScores.add(l1)
    }

    emissionScores
  }

  override def saveX2i(printWriter: PrintWriter): Unit = {
    save(printWriter, TYPE_GREEDY, "inferenceType")
    save(printWriter, inputSize, "inputSize")
    save(printWriter, t2i, "t2i")
  }
}

object GreedyForwardLayer {
  def load(parameters: ParameterCollection,
           x2iIterator: Iterator[String]): GreedyForwardLayer = {
    //
    // load the x2i info
    //
    val byLineIntBuilder = new ByLineIntBuilder()
    val byLineStringMapBuilder = new ByLineStringMapBuilder()

    val inputSize = byLineIntBuilder.build(x2iIterator)
    val t2i = byLineStringMapBuilder.build(x2iIterator)
    val i2t = fromIndexToString(t2i)

    //
    // make the loadable parameters
    //
    val H = parameters.addParameters(Dim(t2i.size, inputSize))

    new GreedyForwardLayer(parameters,
      inputSize, t2i, i2t, H)
  }
}


