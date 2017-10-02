package org.clulab.utils

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
}
