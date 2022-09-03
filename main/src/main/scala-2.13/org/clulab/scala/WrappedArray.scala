package org.clulab.scala

import _root_.scala.collection.immutable.{ArraySeq => ImmutableArraySeq}
import _root_.scala.collection.immutable.{IndexedSeq => ImmutableIndexedSeq}
import _root_.scala.language.implicitConversions

object WrappedArray {

  // Undo the standard conversion to prevent ambiguity from LowPriorityImplicits2 resulting in error
  // "Note that implicit conversions are not applicable because they are ambiguous:".
  // See https://stackoverflow.com/questions/15592324/how-can-an-implicit-be-unimported-from-the-scala-repl.
//  def copyArrayToImmutableIndexedSeq[T](xs: Array[T]): Unit = ???

  // Undo the standard conversion to prevent ambiguity from LowPriorityImplicits resulting in error
  // "Note that implicit conversions are not applicable because they are ambiguous:".
  def genericWrapArray[T](xs: Array[T]): Unit = ???

  // Undo the standard conversion to prevent ambiguity from Predef resulting in error
  // "Note that implicit conversions are not applicable because they are ambiguous:".
  def genericArrayOps[T](xs: Array[T]): Unit = ???

  // TODO: Replace above with something local and fast?

//  implicit def toIndexedSeq[T](array: Array[T]): ImmutableIndexedSeq[T] = {
//    println("Custom conversion!")
//    ImmutableArraySeq.unsafeWrapArray(array)
//  }

  implicit def copyArrayToImmutableIndexedSeq[T](xs: Array[T]): IndexedSeq[T] = {
    println("Custom conversion!")
    if (xs eq null) null
    else ImmutableArraySeq.unsafeWrapArray(xs)
  }
}
