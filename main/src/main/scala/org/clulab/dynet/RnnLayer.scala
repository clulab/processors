package org.clulab.dynet
import java.io.PrintWriter

import edu.cmu.dynet.{Expression, ExpressionVector, GruBuilder, LstmBuilder, ParameterCollection, RnnBuilder}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.ArrayBuffer
import org.clulab.dynet.Utils._
import org.clulab.utils.Configured

/**
 * This layer applies a biLSTM over the sequence of Expressions produced by a previous layer
 * @author Mihai
 */
class RnnLayer (val parameters:ParameterCollection,
                val inputSize: Int,
                val numLayers: Int,
                val rnnStateSize: Int,
                val useHighwayConnections: Boolean,
                val rnnType: String, // "gru" or "lstm"
                val wordFwRnnBuilder:RnnBuilder,
                val wordBwRnnBuilder:RnnBuilder,
                val dropoutProb: Float) extends IntermediateLayer {

  override def forward(inputExpressions: ExpressionVector, doDropout: Boolean): ExpressionVector = {
    setRnnDropout(wordFwRnnBuilder, dropoutProb, doDropout)
    setRnnDropout(wordBwRnnBuilder, dropoutProb, doDropout)
    setRnnDropout(wordBwRnnBuilder, dropoutProb, doDropout)

    assert(inputExpressions.nonEmpty)
    val fwEmbeddings = inputExpressions
    val fwStates = Utils.transduce(fwEmbeddings, wordFwRnnBuilder)
    assert(fwStates.length == inputExpressions.length)
    val bwEmbeddings = fwEmbeddings.reverse
    val bwStates = Utils.transduce(bwEmbeddings, wordBwRnnBuilder).reverse
    assert(bwStates.length == inputExpressions.length)

    // the word state concatenates the fwd and bwd LSTM hidden states; and the input embedding if useHighwayConnections
    val states = fwStates.indices.map { i =>
      if (useHighwayConnections)
        Expression.concatenate(fwStates(i), bwStates(i), inputExpressions(i))
      else
        Expression.concatenate(fwStates(i), bwStates(i))
    }

    states
  }

  override def outDim: Int = {
    val highwaySize = if(useHighwayConnections) inputSize else 0
    2 * rnnStateSize + highwaySize
  }

  override def inDim: Int = inputSize

  override def saveX2i(printWriter: PrintWriter): Unit = {
    save(printWriter, inputSize, "inputSize")
    save(printWriter, numLayers, "numLayers")
    save(printWriter, rnnStateSize, "rnnStateSize")
    save(printWriter, if(useHighwayConnections) 1 else 0, "useHighwayConnections")
    save(printWriter, rnnType, "rnnType")
    save(printWriter, dropoutProb, "dropoutProb")
  }

  override def toString: String = {
    s"RnnLayer($rnnType, $inDim, $outDim)"
  }
}

object RnnLayer {
  val logger: Logger = LoggerFactory.getLogger(classOf[RnnLayer])

  val DEFAULT_DROPOUT_PROB: Float = Utils.DEFAULT_DROPOUT_PROBABILITY

  def load(parameters: ParameterCollection,
           x2iIterator: BufferedIterator[String]): RnnLayer = {
    //
    // load the x2i info
    //
    val byLineIntBuilder = new ByLineIntBuilder()
    val byLineFloatBuilder = new ByLineFloatBuilder()
    val byLineStringBuilder = new ByLineStringBuilder()

    val inputSize = byLineIntBuilder.build(x2iIterator, "inputSize")
    val numLayers = byLineIntBuilder.build(x2iIterator, "numLayers")
    val rnnStateSize = byLineIntBuilder.build(x2iIterator, "rnnStateSize")
    val useHighwayConnectionsAsInt = byLineIntBuilder.build(x2iIterator, "useHighwayConnections")
    val useHighwayConnections = useHighwayConnectionsAsInt == 1
    val rnnType = byLineStringBuilder.build(x2iIterator, "rnnType", defaultValue = "lstm")
    val dropoutProb = byLineFloatBuilder.build(x2iIterator, "dropoutProb")

    //
    // make the loadable parameters
    //
    val fwBuilder = mkBuilder(rnnType, numLayers, inputSize, rnnStateSize, parameters)
    val bwBuilder = mkBuilder(rnnType, numLayers, inputSize, rnnStateSize, parameters)

    new RnnLayer(parameters,
      inputSize, numLayers, rnnStateSize, useHighwayConnections,
      rnnType, fwBuilder, bwBuilder, dropoutProb)
  }

  private def mkBuilder(rnnType: String,
                        numLayers: Int,
                        inputSize: Int,
                        rnnStateSize: Int,
                        parameters: ParameterCollection): RnnBuilder = {
    rnnType match {
      case "gru" => new GruBuilder(numLayers, inputSize, rnnStateSize, parameters)
      case "lstm" => new LstmBuilder(numLayers, inputSize, rnnStateSize, parameters)
      case _ => throw new RuntimeException(s"""ERROR: unknown rnnType "$rnnType"!""")
    }
  }

  def initialize(config: Configured,
                 paramPrefix: String,
                 parameters: ParameterCollection,
                 inputSize: Int): Option[IntermediateLayer] = {
    if (!config.contains(paramPrefix)) {
      return None
    }

    val numLayers = config.getArgInt(paramPrefix + ".numLayers", Some(1))
    val rnnStateSize = config.getArgInt(paramPrefix + ".rnnStateSize", None)
    val useHighwayConnections = config.getArgBoolean(paramPrefix + ".useHighwayConnections", Some(false))
    val rnnType = config.getArgString(paramPrefix + ".type", Some("lstm"))
    val dropoutProb = config.getArgFloat(paramPrefix + ".dropoutProb", Some(RnnLayer.DEFAULT_DROPOUT_PROB))

    val wordFwRnnBuilder = mkBuilder(rnnType, numLayers, inputSize, rnnStateSize, parameters)
    val wordBwRnnBuilder = mkBuilder(rnnType, numLayers, inputSize, rnnStateSize, parameters)

    val layer = new RnnLayer(
      parameters,
      inputSize, numLayers, rnnStateSize, useHighwayConnections,
      rnnType, wordFwRnnBuilder, wordBwRnnBuilder,
      dropoutProb
    )

    Some(layer)
  }
}
