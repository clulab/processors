package org.clulab.scaladynet.parameters

// This is internal
class ParameterInitConst(c: Float) {

}

object ParameterInitConst {

  def apply(c: Float): ParameterInitConst = new ParameterInitConst(c)
}
