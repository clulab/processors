package org.clulab.utils

import scala.jdk.CollectionConverters._

object JavaUtils {

  def asJava[T](scalaIterator: Iterator[T]): java.util.Iterator[T] = {
    scalaIterator.asJava
  }
}
