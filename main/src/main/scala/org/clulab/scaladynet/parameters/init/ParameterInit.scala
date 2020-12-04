package org.clulab.scaladynet.parameters.init

import org.clulab.scaladynet.utils.Tensor

abstract class ParameterInit {
  def initialize_params(values: Tensor)
}
