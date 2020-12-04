package org.clulab.scaladynet.utils

class Dim(protected val values: Seq[Long]) {
  val d: Array[Int] = Array(10)
  val nd = 6

  def get(i: Long): Long = values(i.toInt) // Watch for overflow.

  def apply(i: Int): Int = d(i)
}

object Dim {

  def apply(values: Seq[Long], b: Long = 0): Dim = {
    require(b == 0)
    new Dim(values.map(_.toLong))
  }

  def apply(values: Long*): Dim = apply(values)
}
