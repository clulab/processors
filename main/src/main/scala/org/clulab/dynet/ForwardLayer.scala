package org.clulab.dynet

import edu.cmu.dynet.{Dim, Expression, ExpressionVector, Parameter, ParameterCollection}
import org.slf4j.{Logger, LoggerFactory}
import ForwardLayer._
import org.clulab.dynet.Utils.{ByLineIntBuilder, fromIndexToString, mkTransitionMatrix}
import org.clulab.struct.Counter
import org.clulab.utils.Configured

abstract class ForwardLayer (val parameters:ParameterCollection,
                             val inputSize: Int,
                             val t2i: Map[String, Int],
                             val i2t: Array[String],
                             val H: Parameter,
                             val dropoutProb: Float = DROPOUT_PROB)
  extends FinalLayer {

  def forward(inputExpressions: ExpressionVector,
              predicatePositionOpt: Option[Int],
              doDropout: Boolean): ExpressionVector = {
    val pH = Expression.parameter(H)
    val emissionScores = new ExpressionVector()

    if(predicatePositionOpt.isEmpty) {
      for (i <- inputExpressions.indices) {
        var argExp = inputExpressions(i)

        argExp = Utils.expressionDropout(argExp, dropoutProb, doDropout)

        var l1 = pH * argExp

        l1 = Utils.expressionDropout(l1, dropoutProb, doDropout)
        emissionScores.add(l1)
      }
    }

    else {
      val predPosition = predicatePositionOpt.get
      for(i <- inputExpressions.indices) {
        var argExp = inputExpressions(i)
        var predExp = inputExpressions(predPosition)

        // TODO: dropout before or after concatenate?
        val ss = Utils.expressionDropout(Expression.concatenate(argExp, predExp), dropoutProb, doDropout)

        val l1 = Utils.expressionDropout(pH * ss, dropoutProb, doDropout)

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

  val TYPE_GREEDY_STRING = "greedy"
  val TYPE_VITERBI_STRING = "viterbi"

  def load(parameters: ParameterCollection,
           x2iIterator: Iterator[String]): ForwardLayer = {
    val inferenceType = new ByLineIntBuilder().build(x2iIterator)

    inferenceType match {
      case TYPE_VITERBI => ViterbiForwardLayer.load(parameters, x2iIterator)
      case TYPE_GREEDY => GreedyForwardLayer.load(parameters, x2iIterator)
      case _ => throw new RuntimeException(s"ERROR: unknown forward layer type $inferenceType!")
    }
  }

  def initialize(config: Configured,
                 paramPrefix: String,
                 parameters: ParameterCollection,
                 labelCounter: Counter[String],
                 computeInputSize: (Int) => Int): Option[ForwardLayer] = {
    if (!config.contains(paramPrefix)) {
      return None
    }

    val inferenceType = config.getArgString(paramPrefix + ".inference", Some("greedy"))
    val inputSize = config.getArgInt(paramPrefix + ".inputSize", None)
    val dropoutProb = config.getArgFloat(paramPrefix + ".dropoutProb", Some(ForwardLayer.DROPOUT_PROB))

    val t2i = labelCounter.keySet.toList.sorted.zipWithIndex.toMap
    val i2t = fromIndexToString(t2i)

    val actualInputSize = computeInputSize(inputSize)
    val H = parameters.addParameters(Dim(t2i.size, actualInputSize))

    inferenceType match {
      case TYPE_GREEDY_STRING =>
        Some(new GreedyForwardLayer(parameters, inputSize, t2i, i2t, H, dropoutProb))
      case TYPE_VITERBI_STRING =>
        val T = mkTransitionMatrix(parameters, t2i)
        val layer = new ViterbiForwardLayer(parameters,
          inputSize, t2i, i2t, H, T, dropoutProb)
        layer.initializeTransitions()
        Some(layer)
      case _ =>
        new RuntimeException(s"ERROR: unknown inference type $inferenceType!")
        None
    }
  }
}