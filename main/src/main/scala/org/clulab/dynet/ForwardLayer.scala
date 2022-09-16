package org.clulab.dynet

import edu.cmu.dynet.{Dim, Expression, ExpressionVector, LookupParameter, Parameter, ParameterCollection}
import org.slf4j.{Logger, LoggerFactory}
import org.clulab.dynet.Utils.{ByLineIntBuilder, fromIndexToString, mkTransitionMatrix}
import org.clulab.scala.BufferedIterator
import org.clulab.scala.WrappedArrayBuffer._
import org.clulab.struct.Counter
import org.clulab.utils.Configured

import ForwardLayer._

abstract class ForwardLayer (val parameters:ParameterCollection,
                             val inputSize: Int,
                             val isDual: Boolean,
                             val t2i: Map[String, Int],
                             val i2t: Array[String],
                             val H: Parameter,
                             val rootParam: Parameter,
                             val distanceEmbeddingSize: Int,
                             val distanceLookupParameters: Option[LookupParameter],
                             val nonlinearity: Int,
                             val dropoutProb: Float)
  extends FinalLayer {

  def forward(inputExpressions: ExpressionVector,
              modifierHeadPairs: Option[IndexedSeq[ModifierHeadPair]],
              doDropout: Boolean): ExpressionVector = {
    val pH = Expression.parameter(H)
    val pRoot = Expression.parameter(rootParam)
    val emissionScores = new ExpressionVector()

    if(! isDual) {
      //
      // basic task
      //

      for (i <- inputExpressions.indices) {
        // the hidden state for the argument token
        var argExp = inputExpressions(i)

        // dropout on the hidden state
        argExp = Utils.expressionDropout(argExp, dropoutProb, doDropout)

        // forward layer + dropout on that
        var l1 = Utils.expressionDropout(pH * argExp, dropoutProb, doDropout)

        // the last nonlinearity
        l1 = nonlinearity match {
          case NONLIN_TANH => Expression.tanh(l1)
          case NONLIN_RELU => Expression.rectify(l1)
          case _ => l1
        }

        emissionScores.add(l1)
      }
    }

    else {
      //
      // dual task
      //

      if (modifierHeadPairs.isEmpty) {
        throw new RuntimeException("ERROR: dual task without information about head positions!")
      }

      for (pair <- modifierHeadPairs.get) {
        val modPosition = pair.modifier
        val headPosition = pair.head

        val argExp = Utils.expressionDropout(inputExpressions(modPosition), dropoutProb, doDropout)
        val predExp = if (headPosition >= 0) {
          // there is an explicit head in the sentence
          Utils.expressionDropout(inputExpressions(headPosition), dropoutProb, doDropout)
        } else {
          // the head is root. we use a dedicated Parameter for root
          Utils.expressionDropout(pRoot, dropoutProb, doDropout)
        }

        val ss =
          if (distanceLookupParameters.nonEmpty) {
            val distEmbedding = mkRelativePositionEmbedding(modPosition, headPosition)
            Expression.concatenate(argExp, predExp, distEmbedding)
          } else {
            Expression.concatenate(argExp, predExp)
          }

        var l1 = Utils.expressionDropout(pH * ss, dropoutProb, doDropout)

        l1 = nonlinearity match {
          case NONLIN_TANH => Expression.tanh(l1)
          case NONLIN_RELU => Expression.rectify(l1)
          case _ => l1 // nothing to do otherwise
        }

        emissionScores.add(l1)
      }
    }

    emissionScores
  }

  protected def mkRelativePositionEmbedding(modifier: Int, head: Int): Expression = {
    val dist =
      if(head == -1) { // root
        0
      } else { // head is an actual token
        //println(s" head $head modifier $modifier")
        val fullDist = head - modifier
        if(fullDist < -50) -50
        else if(fullDist > 50) 50
        else fullDist
      } + 50
    //println(s"Finding embedding for position $dist")
    Expression.lookup(distanceLookupParameters.get, dist)
  }

  override def inDim: Int = inputSize

  override def outDim: Int = t2i.size
}

object ForwardLayer {
  val logger: Logger = LoggerFactory.getLogger(classOf[ViterbiForwardLayer])

  val DEFAULT_DROPOUT_PROB: Float = Utils.DEFAULT_DROPOUT_PROBABILITY

  val TYPE_VITERBI = 1
  val TYPE_GREEDY = 2

  val NONLIN_NONE = 0
  val NONLIN_RELU = 1
  val NONLIN_TANH = 2

  val TYPE_GREEDY_STRING = "greedy"
  val TYPE_VITERBI_STRING = "viterbi"

  val DEFAULT_IS_DUAL = 0

  def load(parameters: ParameterCollection,
           x2iIterator: BufferedIterator[String]): ForwardLayer = {
    val inferenceType = new ByLineIntBuilder().build(x2iIterator, "inferenceType")

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
                 isDual: Boolean,
                 inputSize: Int): Option[ForwardLayer] = {
    if (!config.contains(paramPrefix)) {
      return None
    }

    val inferenceType = config.getArgString(paramPrefix + ".inference", Some("greedy"))
    val dropoutProb = config.getArgFloat(paramPrefix + ".dropoutProb", Some(ForwardLayer.DEFAULT_DROPOUT_PROB))

    val nonlinAsString = config.getArgString(paramPrefix + ".nonlinearity", Some(""))
    val nonlin = nonlinAsString match {
      case "relu" => NONLIN_RELU
      case "tanh" => NONLIN_TANH
      case "" => NONLIN_NONE
      case _ => throw new RuntimeException(s"ERROR: unknown non-linearity $nonlinAsString!")
    }

    val t2i = labelCounter.keySet.toList.sorted.zipWithIndex.toMap
    val i2t = fromIndexToString(t2i)

    val distanceEmbeddingSize = config.getArgInt(paramPrefix + ".distanceEmbeddingSize", Some(0))
    val distanceLookupParameters =
      if(distanceEmbeddingSize > 0) Some(parameters.addLookupParameters(101, Dim(distanceEmbeddingSize)))
      else None

    val actualInputSize = if(isDual) 2 * inputSize + distanceEmbeddingSize else inputSize

    val H = parameters.addParameters(Dim(t2i.size, actualInputSize))
    val rootParam = parameters.addParameters(Dim(inputSize))

    inferenceType match {
      case TYPE_GREEDY_STRING =>
        Some(new GreedyForwardLayer(parameters,
          inputSize, isDual,
          t2i, i2t, H, rootParam,
          distanceEmbeddingSize, distanceLookupParameters,
          nonlin, dropoutProb))
      case TYPE_VITERBI_STRING =>
        val T = mkTransitionMatrix(parameters, t2i)
        val layer = new ViterbiForwardLayer(parameters,
          inputSize, isDual,
          t2i, i2t, H, T, rootParam,
          distanceEmbeddingSize, distanceLookupParameters,
          nonlin, dropoutProb)
        layer.initializeTransitions()
        Some(layer)
      case _ =>
        new RuntimeException(s"ERROR: unknown inference type $inferenceType!")
        None
    }
  }
}