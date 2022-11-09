package org.clulab.scala

import _root_.scala.collection.mutable.{ArraySeq => MutableArraySeq}
import _root_.scala.collection.immutable.{ArraySeq => ImmutableArraySeq}
import _root_.scala.collection.immutable.{IndexedSeq => ImmutableIndexedSeq}
import _root_.scala.language.implicitConversions
import scala.collection.ArrayOps

object WrappedArray {

  // Undo the standard conversion to prevent ambiguity from LowPriorityImplicits resulting in error
  // "Note that implicit conversions are not applicable because they are ambiguous:".
  // foreach and zip seem to need this
//  def genericWrapArray[T](xs: Array[T]): MutableArraySeq[T] = {
    // println("This shouldn't happen!")
//    Predef.genericWrapArray(xs)
//  }

  // Undo the standard conversion to prevent ambiguity from Predef resulting in error
  // "Note that implicit conversions are not applicable because they are ambiguous:".
  // This causes a problem with Array.indices
  // foreach and zip seem to need this
//  def genericArrayOps[T](xs: Array[T]): ArrayOps[T] = {
    // println("This shouldn't happen!")
//    Predef.genericArrayOps(xs)
//  }

//  implicit def ImmutableArraySeqToArray[T](immutableArraySeq: ImmutableArraySeq[T]): Array[T] = {
//    println("Custom conversion!")
//    immutableArraySeq.unsafeArray
//  }

  implicit def copyArrayToImmutableIndexedSeq[T](xs: Array[T]): ImmutableArraySeq[T] = {
    // println("Custom conversion!")
    if (xs eq null) null
    else ImmutableArraySeq.unsafeWrapArray(xs)
  }
}
