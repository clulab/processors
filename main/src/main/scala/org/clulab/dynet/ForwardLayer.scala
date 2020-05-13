package org.clulab.dynet

import edu.cmu.dynet.{Parameter, ParameterCollection}
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

  override def inDim: Int = inputSize

  override def outDim: Int = t2i.size
}

object ForwardLayer {
  val logger: Logger = LoggerFactory.getLogger(classOf[ViterbiForwardLayer])

  val DROPOUT_PROB = 0.2f

  val TYPE_VITERBI = 1
  val TYPE_GREEDY = 2
  val TYPE_SRL = 3

  def load(parameters: ParameterCollection,
           x2iIterator: Iterator[String]): ForwardLayer = {
    val inferenceType = new ByLineIntBuilder().build(x2iIterator)

    inferenceType match {
      case TYPE_VITERBI => ViterbiForwardLayer.load(parameters, x2iIterator)
      case TYPE_GREEDY => GreedyForwardLayer.load(parameters, x2iIterator)
      case TYPE_SRL => SrlForwardLayer.load(parameters, x2iIterator)
      case _ => throw new RuntimeException(s"ERROR: unknown forward layer type ${inferenceType}!")
    }
  }
}