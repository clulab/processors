package org.clulab.scaladynet.vectors

class FloatVector(values: Seq[Float] = Seq.empty) extends ImmutableVector[Float](values) {

  def mkString(sep: String): String = values.mkString(sep)

  def toArray: Array[Float] = values.toArray
}

object FloatVector {

  def apply(values: Seq[Float], offset: Int = 0): FloatVector = {
    if (offset == 0) new FloatVector(values)
    else new FloatVector(values.slice(offset, values.length))
  }
}