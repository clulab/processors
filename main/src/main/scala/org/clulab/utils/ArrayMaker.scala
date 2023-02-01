package org.clulab.utils

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayBuilder

object ArrayMaker {

  def buffer[T](f: (ArrayBuffer[T]) => Unit): Array[T] = {
    val arrayBuffer = new ArrayBuffer[T]()

    f(arrayBuffer)
    arrayBuffer.toArray
  }

  def buffer[T](elems: T*)(f: (ArrayBuffer[T]) => Unit): Array[T] = {
    val arrayBuffer = ArrayBuffer[T](elems: _*)

    f(arrayBuffer)
    arrayBuffer.toArray
  }

  def build[T](f: (ArrayBuilder[T]) => Unit): Array[T] = {
    val arrayBuilder = ArrayBuilder.make[T]

    f(arrayBuilder)
    arrayBuilder.result()
  }
}
