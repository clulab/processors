package org.clulab.dynet

import edu.cmu.dynet.{Dim, Expression, ExpressionVector, Parameter, ParameterCollection}
import org.slf4j.{Logger, LoggerFactory}
import ForwardLayer._
import org.clulab.dynet.Utils.{ByLineIntBuilder, fromIndexToString, mkTransitionMatrix}
import org.clulab.struct.Counter
import org.clulab.utils.Configured

import scala.collection.mutable.ArrayBuffer
import org.clulab.struct.EdgeMap
import scala.util.Random
import edu.cmu.dynet.LookupParameter

abstract class ForwardLayer (val parameters:ParameterCollection,
                             val inputSize: Int,
                             val taskType: Int,
                             val t2i: Map[String, Int],
                             val i2t: Array[String],
                             val H: Parameter,
                             val rootParam: Parameter,
                             val positionLookupParameters: LookupParameter, 
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

  val graphRand = new Random(Utils.RANDOM_SEED)

  def forward(inputExpressions: ExpressionVector,
              headPositionsOpt: Option[IndexedSeq[Int]],
              doDropout: Boolean): ExpressionVector = {
    val pH = Expression.parameter(H)
    val pRoot = Expression.parameter(rootParam)
    val emissionScores = new ExpressionVector()

    if(TaskManager.isBasic(taskType)) {
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
        l1 = applyNonlinearity(l1)

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
        emissionScores.add(runForwardDual(i, headPosition, inputExpressions, doDropout))        
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
    Expression.lookup(positionLookupParameters, dist)
  }

  protected def runForwardDual(modifier: Int, 
                               head: Int, 
                               inputExpressions: ExpressionVector,
                               doDropout: Boolean): Expression = {
    val pH = Expression.parameter(H)
    val pRoot = Expression.parameter(rootParam)

    val argExp = Utils.expressionDropout(pickSpan(inputExpressions(modifier)), dropoutProb, doDropout)
    val predExp = if(head >= 0) {
      // there is an explicit head in the sentence
      Utils.expressionDropout(pickSpan(inputExpressions(head)), dropoutProb, doDropout)
    } else {
      // the head is root. we used a dedicated Parameter for root
      Utils.expressionDropout(pickSpan(pRoot), dropoutProb, doDropout)
    }

    val posEmbed = mkRelativePositionEmbedding(modifier, head)

    val ss = Expression.concatenate(argExp, predExp, posEmbed)

    var l1 = Utils.expressionDropout(pH * ss, dropoutProb, doDropout)

    applyNonlinearity(l1)
  }

  protected def applyNonlinearity(e: Expression): Expression = {
    nonlinearity match {
      case NONLIN_TANH => Expression.tanh(e)
      case NONLIN_RELU => Expression.rectify(e)
      case _ => e // nothing to do otherwise
    }
  }

  override def inDim: Int = if(spans.nonEmpty) spanLength(spans.get) else inputSize

  override def outDim: Int = t2i.size

  override def graphLoss(emissionScoresAsExpression: ExpressionVector, goldLabels: IndexedSeq[String]): Expression = {
    throw new RuntimeException("ERROR: graphLoss not supported for this final layer!")
  }

  override def graphForward(inputExpressions: ExpressionVector, 
                            headPositionsOpt: Option[IndexedSeq[Int]],
                            doDropout: Boolean): ExpressionVector = {
    throw new RuntimeException("ERROR: graphForward not supported for this final layer!")
  }

  override def graphInference(emissionScores: EdgeMap[Expression]): EdgeMap[String] = {
    throw new RuntimeException("ERROR: graphInference not supported for this final layer!")
  }

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

  val DEFAULT_BASIC = 0

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
    spans
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
                 taskType: Int,
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
    // println("t2i: " + t2i.toString())

    val spanConfig = config.getArgString(paramPrefix + ".span", Some(""))
    val span =
      if(spanConfig.isEmpty) {
        None
      } else {
        val spans = parseSpan(spanConfig, inputSize)
        // println(s"SPANS = ${spans.mkString(", ")}")
        Some(spans)
      }

    val needsDoubleLength = ! TaskManager.isBasic(taskType)
    val actualInputSize =
      if(span.nonEmpty) {
        val len = spanLength(span.get)
        if(needsDoubleLength) 2 * len else len
      } else {
        if(needsDoubleLength) 2 * inputSize + 32 else inputSize // (2 * inputSize + 32) else inputSize // TODO: implement properly
      }
    // println(s"ACTUAL INPUT SIZE: $actualInputSize")

    val H = parameters.addParameters(Dim(t2i.size, actualInputSize))
    val rootParam = parameters.addParameters(Dim(inputSize))
    val positionLookupParameters = parameters.addLookupParameters(101, Dim(32))

    inferenceType match {
      case TYPE_GREEDY_STRING =>
        Some(new GreedyForwardLayer(parameters,
          inputSize, taskType,
          t2i, i2t, H, rootParam, positionLookupParameters, 
          span, nonlin, dropoutProb))
      case TYPE_VITERBI_STRING =>
        val T = mkTransitionMatrix(parameters, t2i)
        val layer = new ViterbiForwardLayer(parameters,
          inputSize, taskType,
          t2i, i2t, H, T, rootParam, positionLookupParameters, 
          span, nonlin, dropoutProb)
        layer.initializeTransitions()
        Some(layer)
      case _ =>
        new RuntimeException(s"ERROR: unknown inference type $inferenceType!")
        None
    }
  }
}