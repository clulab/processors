package org.clulab.utils

import scala.collection.mutable.ArrayBuffer

object BufferMaker {

  def fill[T](f: (ArrayBuffer[T]) => Unit): ArrayBuffer[T] = {
    val arrayBuffer = new ArrayBuffer[T]()

    f(arrayBuffer)
    arrayBuffer
  }

  def fill[T](elems: T*)(f: (ArrayBuffer[T]) => Unit): ArrayBuffer[T] = {
    val arrayBuffer = ArrayBuffer[T](elems: _*)

    f(arrayBuffer)
    arrayBuffer
  }
}
