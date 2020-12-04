package org.clulab.scaladynet.utils

class Dim(protected val values: Seq[Long]) {

  def get(i: Long): Long = values(i.toInt) // Watch for overflow.
}

object Dim {

  def apply(values: Seq[Long], b: Long = 0): Dim = {
    require(b == 0)
    new Dim(values.map(_.toLong))
  }

  def apply(values: Long*): Dim = apply(values)
}
