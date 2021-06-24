package org.clulab.utils

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

/**
  * Utility functions for manipulating sequences
  * Created by mihais on 8/28/17.
  */
object SeqUtils {
  def revert[T](orig:Seq[T]): Seq[T] = {
    val reverse = new ListBuffer[T]
    for(o <- orig) reverse.insert(0, o)
    reverse
  }

  def indexesOf[T](values: IndexedSeq[T], value: T): IndexedSeq[Int] = {
    val indexes = new ArrayBuffer[Int]()
    var done = false
    var offset = 0

    while (!done) {
      val index = values.indexOf(value, offset)

      if (index >= 0) {
        indexes += index
        offset = index + 1
      }
      else
        done = true
    }
    indexes.toArray
  }
}
