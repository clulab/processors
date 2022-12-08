package org.clulab.scala

import _root_.scala.collection.mutable.{ArraySeq => MutableArraySeq}
import _root_.scala.collection.immutable.{ArraySeq => ImmutableArraySeq}
import _root_.scala.collection.immutable.{IndexedSeq => ImmutableIndexedSeq}
import _root_.scala.language.implicitConversions
import scala.collection.ArrayOps

object WrappedArray {

  implicit def copyArrayToImmutableIndexedSeq[T](xs: Array[T]): ImmutableArraySeq[T] = {
    // println("Custom conversion!")
    if (xs eq null) null
    else ImmutableArraySeq.unsafeWrapArray(xs)
  }
}
