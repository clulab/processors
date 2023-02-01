package org.clulab.utils

import scala.collection.mutable.ListBuffer

object ListMaker {

  def buffer[T](f: (ListBuffer[T]) => Unit): List[T] = {
    val listBuffer = new ListBuffer[T]()

    f(listBuffer)
    listBuffer.toList
  }
}
