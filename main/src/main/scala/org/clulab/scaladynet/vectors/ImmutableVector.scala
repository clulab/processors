package org.clulab.scaladynet.vectors

class ImmutableVector[T](values: Seq[T]) extends Vector[T] {

  override def length: Int = values.length
}
