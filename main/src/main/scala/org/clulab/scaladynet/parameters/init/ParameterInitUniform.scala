package org.clulab.scaladynet.parameters.init
import org.clulab.scaladynet.utils.Tensor
import org.clulab.scaladynet.utils.TensorTools

// This is internal
class ParameterInitUniform(scale: Float) extends ParameterInit {

  override def initialize_params(values: Tensor): Unit =
      TensorTools.randomize_uniform(values, -scale, scale)
}

object ParameterInitUniform {

  def apply(scale: Float): ParameterInitUniform = new ParameterInitUniform(scale)
}

