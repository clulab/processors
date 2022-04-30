package org.clulab.utils

import collection.JavaConverters._

object JavaUtils {

  def asJava[T](scalaIterator: Iterator[T]): java.util.Iterator[T] = {
    scalaIterator.asJava
  }
}
