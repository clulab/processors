package org.clulab.dynet
import java.io.PrintWriter

import edu.cmu.dynet.{Expression, ExpressionVector, LstmBuilder, Parameter, ParameterCollection}
import org.clulab.dynet.Utils.save
import org.clulab.utils.Configured

class LinearLayer(val parameters: ParameterCollection,
                  val inputSize: Int,
                  val outputSize: Int,
                  val W: Parameter,
                  val b: Parameter) extends IntermediateLayer {
  override def forward(inputExpressions: ExpressionVector, doDropout: Boolean): ExpressionVector = {
    val wp = Expression.parameter(W)
    val bp = Expression.parameter(b)
    val z = for (e <- inputExpressions) yield e * wp + bp
    ExpressionVector.Seq2ExpressionVector(z)
  }

  override def inDim: Int = inputSize

  override def outDim: Int = outputSize

  override def saveX2i(printWriter: PrintWriter): Unit = {
    save(printWriter, inputSize, "inputSize")
    save(printWriter, outputSize, "outputSize")
  }
}
object LinearLayer {
  def initialize(config: Configured,
                 paramPrefix: String,
                 parameters: ParameterCollection,
                 inputSize: Int): Option[IntermediateLayer] = {
    if (!config.contains(paramPrefix)) {
      return None
    }

    //    val numLayers = config.getArgInt(paramPrefix + ".numLayers", Some(1))
    val outputSize = config.getArgInt(paramPrefix + ".outputSize", None)

    val layer = new LinearLayer(parameters, inputSize, outputSize)

    Some(layer)
  }
}
