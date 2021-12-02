package org.clulab.utils

import scala.collection.mutable

// Array.view(from, until) is no longer available in Scala 2.13+.
class ArrayView[T](array: Array[T], from: Int, until: Int) extends IndexedSeq[T] with mutable.IndexedSeq[T] {

  override def apply(index: Int): T = array(from + index)

  override def length: Int = until - from

  override def update(index: Int, elem: T): Unit = array(from + index) = elem
}

object ArrayView {

  def apply[T](array: Array[T], from: Int): ArrayView[T] = apply(array, from, array.length)

  def apply[T](array: Array[T], from: Int, until: Int): ArrayView[T] = new ArrayView(array, from, until)
}
