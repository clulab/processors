package org.clulab.scaladynet.parameters.init
import org.clulab.scaladynet.utils.Tensor
import org.clulab.scaladynet.utils.TensorTools

class ParameterInitGlorot(is_lookup: Boolean, gain: Float) extends ParameterInit {

  override def initialize_params(values: Tensor): Unit = {
    val dim_len = values.d.nd - (if (is_lookup) 1 else 0)
    val my_scale =  if (dim_len == 4) {
      // When doing a Conv the parameters is (H, W, In, Out)
      val receptive_field = values.d(0) * values.d(1)
      // Other framework m + n are calculated by multiplying by the kernel size.
      val dims = values.d(2) * receptive_field + values.d(3) * receptive_field
      gain * math.sqrt(6) / math.sqrt(dims)
    }
    else {
      val dims = values.d.d.sum
      gain * math.sqrt(3 * dim_len) / math.sqrt(dims)
    }
    TensorTools.randomize_uniform(values, -my_scale, my_scale)
  }
}

object ParameterInitGlorot {

  def apply(is_lookup: Boolean = false, gain: Float = 1f): ParameterInitGlorot =
      new ParameterInitGlorot(is_lookup, gain)
}
