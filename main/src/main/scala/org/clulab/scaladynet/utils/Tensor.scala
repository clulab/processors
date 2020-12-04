package org.clulab.scaladynet.utils

import org.clulab.scaladynet.vectors.FloatVector

class Tensor {
  val d: Dim = Dim(5) // wrong

  protected def float: Float = 0f

  def toFloat(): Float = float

  def toVector(): FloatVector = new FloatVector(Seq(float))
}
