package org.clulab.utils

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Buffer {

  def makeArray[T](f: (ArrayBuffer[T]) => Unit): ArrayBuffer[T] = {
    val arrayBuffer = new ArrayBuffer[T]()

    f(arrayBuffer)
    arrayBuffer
  }

  def makeList[T](f: (ListBuffer[T]) => Unit): ListBuffer[T] = {
    null
  }

  def fillArray[T](elems: T*)(f: (ArrayBuffer[T]) => Unit): ArrayBuffer[T] = {
    val arrayBuffer = ArrayBuffer[T](elems: _*)

    f(arrayBuffer)
    arrayBuffer
  }

  def fillList[T](elems: T*)(f: (ListBuffer[T]) => Unit): ListBuffer[T] = {
    null
  }
}
