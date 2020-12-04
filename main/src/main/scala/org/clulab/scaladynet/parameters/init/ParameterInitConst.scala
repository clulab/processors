package org.clulab.scaladynet.parameters.init
import org.clulab.scaladynet.utils.Tensor
import org.clulab.scaladynet.utils.TensorTools

// This is internal
class ParameterInitConst(cnst: Float) extends ParameterInit {

  override def initialize_params(values: Tensor): Unit = TensorTools.constant(values, cnst)
}

object ParameterInitConst {

  def apply(c: Float): ParameterInitConst = new ParameterInitConst(c)
}
