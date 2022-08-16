package org.clulab.scala

//import scala.collection.{Seq => GenericSeq}
import scala.collection.immutable.{IndexedSeq => ImmutableIndexedSeq}
import scala.collection.mutable.{ListBuffer => MutableListBuffer}
import scala.language.implicitConversions

class WrappedListBuffer[T](listBuffer: MutableListBuffer[T]) extends ImmutableIndexedSeq[T] {

  override def apply(i: Int): T = listBuffer(i)

  override def length: Int = listBuffer.length
}

object WrappedListBuffer {

  // The version without the underscore causes havoc!
  implicit def _toImmutableIndexedSeq[T](listBuffer: MutableListBuffer[T]): ImmutableIndexedSeq[T] = {
    new WrappedListBuffer(listBuffer)
  }

//  implicit def toSeq[T](listBuffer: MutableListBuffer[T]): GenericSeq[T] = {
//    new WrappedListBuffer(listBuffer)
//  }
}
