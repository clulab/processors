package org.clulab.dynet

import java.io.PrintWriter

import edu.cmu.dynet.{Dim, Expression, ExpressionVector, Parameter, ParameterCollection}
import org.clulab.dynet.Utils._
import ForwardLayer._

class SrlForwardLayer (parameters:ParameterCollection,
                       inputSize: Int,
                       t2i: Map[String, Int],
                       i2t: Array[String],
                       H: Parameter,
                       dropoutProb: Float = DROPOUT_PROB)
  extends ForwardLayer(parameters, inputSize, t2i, i2t, H, dropoutProb) {

  override def loss(finalStates: ExpressionVector, goldLabels: IndexedSeq[Int]): Expression = {
    sentenceLossGreedy(finalStates, goldLabels)
  }

  override def forward(inputExpressions: ExpressionVector,
                       predicatePositionOpt: Option[Int],
                       doDropout: Boolean): ExpressionVector = {
    val pH = Expression.parameter(H)
    val emissionScores = new ExpressionVector()

    // concatenate the state of the predicate and argument
    assert(predicatePositionOpt.nonEmpty)
    val predPosition = predicatePositionOpt.get
    for(i <- inputExpressions.indices) {
      var argExp = inputExpressions(i)
      var predExp = inputExpressions(predPosition)

      if(doDropout) {
        argExp = Expression.dropout(argExp, dropoutProb)
        predExp = Expression.dropout(predExp, dropoutProb)
      }

      val ss = Expression.concatenate(argExp, predExp)
      var l1 = pH * ss
      if(doDropout) {
        l1 = Expression.dropout(l1, dropoutProb)
      }
      emissionScores.add(l1)
    }

    emissionScores
  }

  override def saveX2i(printWriter: PrintWriter): Unit = {
    save(printWriter, TYPE_SRL, "inferenceType")
    save(printWriter, inputSize, "inputSize")
    save(printWriter, t2i, "t2i")
  }
}

object SrlForwardLayer {
  def load(parameters: ParameterCollection,
           x2iIterator: Iterator[String]): SrlForwardLayer = {
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

    new SrlForwardLayer(parameters,
      inputSize, t2i, i2t, H)
  }
}


