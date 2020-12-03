package org.clulab.scaladynet.utils

class Dim {
  val counts: Seq[Int] = Seq(1)
  def get(i: Long): Long = ???
}

object Dim {
  def apply(values: Int*): Dim = ???
}
