package org.clulab.dynet

import scala.collection.mutable

/**
 * Produces a thread-safe pointer to an object, by cloning a reference object once per thread
 */
class Threadable[T <: Cloneable](val reference: T) {
  val objectsPerThread = new mutable.HashMap[Long, T]()

  def get: T = this.synchronized {
    objectsPerThread.getOrElseUpdate(Thread.currentThread().getId, reference.clone().asInstanceOf[T])
  }
}
