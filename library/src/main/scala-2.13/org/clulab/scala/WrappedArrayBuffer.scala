package org.clulab.scala

//import scala.collection.{Seq => GenericSeq}
import scala.collection.immutable.{IndexedSeq => ImmutableIndexedSeq}
import scala.collection.mutable.{ArrayBuffer => MutableArrayBuffer}
import scala.language.implicitConversions

class WrappedArrayBuffer[T](arrayBuffer: MutableArrayBuffer[T]) extends ImmutableIndexedSeq[T] {

  override def apply(i: Int): T = arrayBuffer(i)

  override def length: Int = arrayBuffer.length
}

object WrappedArrayBuffer {

  // The version without the underscore causes havoc!
  implicit def _toIndexedSeq[T](arrayBuffer: MutableArrayBuffer[T]): ImmutableIndexedSeq[T] = {
    new WrappedArrayBuffer(arrayBuffer)
  }

//  implicit def toSeq[T](arrayBuffer: MutableArrayBuffer[T]): GenericSeq[T] = {
//    new WrappedArrayBuffer(arrayBuffer)
//  }
}
