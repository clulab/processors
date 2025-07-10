package org.clulab.utils

import scala.collection.mutable
import scala.collection.compat.immutable.ArraySeq

class WrappedArraySeq[T](array: Array[T]) {
  def toSeq: Seq[T] = toImmutableSeq

  def toMutableSeq: mutable.Seq[T] = {
    array
  }

  def toImmutableSeq: Seq[T] = {
    ArraySeq.unsafeWrapArray(array)
  }
}

object WrappedArraySeq {

  def apply[T](array: Array[T]): WrappedArraySeq[T] = new WrappedArraySeq(array)
}
