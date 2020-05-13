package org.clulab.dynet

import edu.cmu.dynet.{Expression, ExpressionVector, Parameter, ParameterCollection}
import org.slf4j.{Logger, LoggerFactory}
import ForwardLayer._
import org.clulab.dynet.Utils.ByLineIntBuilder

abstract class ForwardLayer (val parameters:ParameterCollection,
                             val inputSize: Int,
                             val t2i: Map[String, Int],
                             val i2t: Array[String],
                             val H: Parameter,
                             val dropoutProb: Float = DROPOUT_PROB)
  extends FinalLayer with Saveable {

  def forward(inputExpressions: ExpressionVector,
              predicatePositionOpt: Option[Int],
              doDropout: Boolean): ExpressionVector = {
    val pH = Expression.parameter(H)
    val emissionScores = new ExpressionVector()

    if(predicatePositionOpt.isEmpty) {
      for (i <- inputExpressions.indices) {
        var argExp = inputExpressions(i)

        if(doDropout) {
          // TODO: there was no dropout here in the old system
          argExp = Expression.dropout(argExp, dropoutProb)
        }

        var l1 = pH * argExp

        if (doDropout) {
          l1 = Expression.dropout(l1, dropoutProb)
        }
        emissionScores.add(l1)
      }
    }

    else {
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
    }

    emissionScores
  }

  override def inDim: Int = inputSize

  override def outDim: Int = t2i.size
}

object ForwardLayer {
  val logger: Logger = LoggerFactory.getLogger(classOf[ViterbiForwardLayer])

  val DROPOUT_PROB = 0.2f

  val TYPE_VITERBI = 1
  val TYPE_GREEDY = 2

  def load(parameters: ParameterCollection,
           x2iIterator: Iterator[String]): ForwardLayer = {
    val inferenceType = new ByLineIntBuilder().build(x2iIterator)

    inferenceType match {
      case TYPE_VITERBI => ViterbiForwardLayer.load(parameters, x2iIterator)
      case TYPE_GREEDY => GreedyForwardLayer.load(parameters, x2iIterator)
      case _ => throw new RuntimeException(s"ERROR: unknown forward layer type ${inferenceType}!")
    }
  }
}