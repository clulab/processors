package org.clulab.scaladynet.vectors

class FloatVector(values: Seq[Float] = Seq.empty) extends ImmutableVector[Float](values) {

  def mkString(sep: String): String = values.mkString(sep)

  def toArray: Array[Float] = values.toArray
}
