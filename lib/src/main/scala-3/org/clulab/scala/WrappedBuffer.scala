package org.clulab.scala

import scala.collection.immutable.{IndexedSeq => ImmutableIndexedSeq}
import scala.collection.mutable.{Buffer => MutableBuffer}
import scala.language.implicitConversions

class WrappedBuffer[T](buffer: MutableBuffer[T]) extends ImmutableIndexedSeq[T] {

  override def apply(i: Int): T = buffer(i)

  override def length: Int = buffer.length
}

object WrappedBuffer {

  // The version without the underscore causes havoc!
  implicit def _toIndexedSeq[T](buffer: MutableBuffer[T]): ImmutableIndexedSeq[T] = {
    new WrappedBuffer(buffer)
  }
}
