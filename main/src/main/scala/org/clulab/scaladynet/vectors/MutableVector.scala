package org.clulab.scaladynet.vectors

import scala.collection.mutable

abstract class MutableVector[T](protected val values: mutable.IndexedSeq[T]) extends Vector {

  def update(idx: Int, elem: T): Unit = values(idx) = elem

  override def length: Int = values.length
}
