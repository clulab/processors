package org.clulab.utils

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayBuilder
import scala.reflect.ClassTag

object ArrayMaker {

  def buffer[T: ClassTag](f: (ArrayBuffer[T]) => Unit): Array[T] = {
    val arrayBuffer = new ArrayBuffer[T]()

    f(arrayBuffer)
    arrayBuffer.toArray
  }

  def bufferLen[T: ClassTag](len: Int)(f: (ArrayBuffer[T]) => Unit): Array[T] = {
    val arrayBuffer = new ArrayBuffer[T](len)

    f(arrayBuffer)
    arrayBuffer.toArray
  }

  def build[T: ClassTag](f: (ArrayBuilder[T]) => Unit): Array[T] = {
    val arrayBuilder = ArrayBuilder.make[T]

    f(arrayBuilder)
    arrayBuilder.result()
  }
}
