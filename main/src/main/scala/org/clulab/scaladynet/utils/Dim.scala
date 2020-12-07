package org.clulab.scaladynet.utils

class Dim(val d: Seq[Int] /* Array of dimension */, val bd: Int /* Batch dimension */) {
  val nd = d.size // Number of dimensions
  val batchSize = d.foldLeft(1)(_ * _)

  // Batch dimension
  def batch_elems(): Int = bd

  def get(i: Int): Int = d(i)

  def apply(i: Int): Int = d(i)

  // Total size of a batch
  def size(): Int = batch_size() * bd

  // Size of a batch (product of all dimensions)
  def batch_size(): Int = batchSize
}

object Dim {

  def apply(): Dim = apply(Seq.empty, 1)

  def apply(values: Int*): Dim = apply(values, 1)

  def apply(other: Dim, bd: Int): Dim = apply(other.d, bd)

  def apply(values: Seq[Int], b: Int): Dim = new Dim(values, b)
}
