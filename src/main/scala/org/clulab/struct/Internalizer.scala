package org.clulab.struct

import java.text.DecimalFormat

import scala.collection.mutable

/**
 * Replicates the functionality of Java's String.intern() but using heap memory and any object type not just String
 * User: mihais
 * Date: 3/1/13
 */
class Internalizer[T] {
  val records = new mutable.HashMap[T, T]()

  /** Counts the number of objects that were saved by not storing them repeatedly */
  private var savedObjects = 0

  /**
   * Interns the object t. That is, if the object was already seen once, it returns a pointer to that object to avoid replication.
   * @param t
   * @return
   */
  def intern(t:T): T = {
    if (records.contains(t)) {
      savedObjects += 1
      records.get(t).get
    } else {
      records += t -> t
      t
    }
  }

  def stats() {
    val formatter = new DecimalFormat("#.##")
    println("Stored objects: " + records.size)
    println(" Saved objects: " + formatter.format((100.0 * savedObjects) / (savedObjects + records.size)) + "%")
  }

  def clear(showStats:Boolean = false) { if(showStats) stats(); records.clear() }
}
