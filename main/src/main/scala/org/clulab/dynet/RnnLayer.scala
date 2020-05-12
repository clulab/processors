package org.clulab.dynet
import java.io.PrintWriter

import edu.cmu.dynet.{ExpressionVector, ParameterCollection, RnnBuilder}
import org.slf4j.{Logger, LoggerFactory}

/**
 * This layer applies a biLSTM over the sequence of Expressions produced by a previous layer
 * @author Mihai
 */
class RnnLayer (val parameters:ParameterCollection,
                val inputSize: Int,
                val rnnStateSize: Int,
                val useHighwayConnections: Boolean,
                val wordFwRnnBuilder:RnnBuilder,
                val wordBwRnnBuilder:RnnBuilder,
                val dropoutProb: Float = RnnLayer.DROPOUT_PROB) extends IntermediateLayer with Saveable {

  override def mkEmbeddings(inputExpressions: ExpressionVector, doDropout: Boolean): ExpressionVector = {

  }

  override def outDim: Int = {
    val highwaySize = if(useHighwayConnections) inputSize else 0
    2 * rnnStateSize + highwaySize
  }

  override def inDim: Int = inputSize

  override def saveX2i(printWriter: PrintWriter): Unit = {

  }
}

object RnnLayer {
  val logger: Logger = LoggerFactory.getLogger(classOf[RnnLayer])

  val DROPOUT_PROB = 0.2f

  def load(parameters: ParameterCollection,
           x2iIterator: Iterator[String]): RnnLayer = {

  }
}
