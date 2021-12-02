package org.clulab.dynet

import edu.cmu.dynet.{Dim, Expression, ExpressionVector, Parameter, ParameterCollection}
import org.slf4j.{Logger, LoggerFactory}
import ForwardLayer._
import org.clulab.dynet.Utils.{ByLineIntBuilder, fromIndexToString, mkTransitionMatrix}
import org.clulab.struct.Counter
import org.clulab.utils.Configured

import scala.collection.mutable.ArrayBuffer

abstract class ForwardLayer (val parameters:ParameterCollection,
                             val inputSize: Int,
                             val isDual: Boolean,
                             val t2i: Map[String, Int],
                             val i2t: Array[String],
                             val H: Parameter,
                             val rootParam: Parameter,
                             val spans: Option[Seq[(Int, Int)]],
                             val nonlinearity: Int,
                             val dropoutProb: Float)
  extends FinalLayer {

  def pickSpan(v: Expression): Expression = {
    if(spans.isEmpty) {
      v
    } else {
      val vs = new ExpressionVector()
      for(span <- spans.get) {
        val e = Expression.pickrange(v, span._1, span._2, 0)
        vs.add(e)
      }
      Expression.concatenate(vs)
    }
  }

  def forward(inputExpressions: ExpressionVector,
              headPositionsOpt: Option[IndexedSeq[Int]],
              doDropout: Boolean): ExpressionVector = {
    val pH = Expression.parameter(H)
    val pRoot = Expression.parameter(rootParam)
    val emissionScores = new ExpressionVector()

    if(! isDual) {
      //
      // basic task
      //

      for (i <- inputExpressions.indices) {
        // fetch the relevant span from the RNN's hidden state
        var argExp = pickSpan(inputExpressions(i))

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

      if(headPositionsOpt.isEmpty) {
        throw new RuntimeException("ERROR: dual task without information about head positions!")
      }

      for(i <- inputExpressions.indices) {
        val headPosition = headPositionsOpt.get(i)

        val argExp = Utils.expressionDropout(pickSpan(inputExpressions(i)), dropoutProb, doDropout)
        val predExp = if(headPosition >= 0) {
          // there is an explicit head in the sentence
          Utils.expressionDropout(pickSpan(inputExpressions(headPosition)), dropoutProb, doDropout)
        } else {
          // the head is root. we used a dedicated Parameter for root
          Utils.expressionDropout(pickSpan(pRoot), dropoutProb, doDropout)
        }

        val ss = Expression.concatenate(argExp, predExp)

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

  override def inDim: Int = if(spans.nonEmpty) spanLength(spans.get) else inputSize

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

  def parseSpan(spanParam: String, inputSize: Int): Seq[(Int, Int)] = {
    val spans = new ArrayBuffer[(Int, Int)]()
    val spanParamTokens = spanParam.split(",")
    for(spanParamToken <- spanParamTokens) {
      val spanTokens = spanParamToken.split('-')
      assert(spanTokens.length == 2)
      val start = spanTokens(0).toInt // these are actual values, not percentages as in the config file
      val end = spanTokens(1).toInt
      spans += Tuple2(start, end)
    }
    spans.toSeq
  }

  /** Produces a string representation of spans, which can be parsed by parseSpan */
  def spanToString(spans: Seq[(Int, Int)]): String = {
    val sb = new StringBuilder
    var first = true
    for(span <- spans) {
      if(! first) sb.append(",")
      sb.append(span._1)
      sb.append("-")
      sb.append(span._2)
      first = false
    }
    sb.toString()
  }

  def spanLength(spans: Seq[(Int, Int)]): Int = {
    var sum = 0
    for(x <- spans) {
      sum += x._2 - x._1
    }
    sum
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

    val spanConfig = config.getArgString(paramPrefix + ".span", Some(""))
    val span =
      if(spanConfig.isEmpty) {
        None
      } else {
        val spans = parseSpan(spanConfig, inputSize)
        // println(s"SPANS = ${spans.mkString(", ")}")
        Some(spans)
      }

    val actualInputSize =
      if(span.nonEmpty) {
        val len = spanLength(span.get)
        if(isDual) 2 * len else len
      } else {
        if(isDual) 2 * inputSize else inputSize
      }
    // println(s"ACTUAL INPUT SIZE: $actualInputSize")

    val H = parameters.addParameters(Dim(t2i.size, actualInputSize))
    val rootParam = parameters.addParameters(Dim(inputSize))

    inferenceType match {
      case TYPE_GREEDY_STRING =>
        Some(new GreedyForwardLayer(parameters,
          inputSize, isDual,
          t2i, i2t, H, rootParam,
          span, nonlin, dropoutProb))
      case TYPE_VITERBI_STRING =>
        val T = mkTransitionMatrix(parameters, t2i)
        val layer = new ViterbiForwardLayer(parameters,
          inputSize, isDual,
          t2i, i2t, H, T, rootParam,
          span, nonlin, dropoutProb)
        layer.initializeTransitions()
        Some(layer)
      case _ =>
        new RuntimeException(s"ERROR: unknown inference type $inferenceType!")
        None
    }
  }
}