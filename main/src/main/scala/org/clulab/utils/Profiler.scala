package org.clulab.utils

import collection.mutable.ArrayBuffer

/**
 * Simple code profiler
 * User: peter
 * Date: 5/15/13
 */

object Profiler {
  val startTimes = new ArrayBuffer[BigInt]()
  val deltas = new ArrayBuffer[BigInt]()
  val numSamples = new ArrayBuffer[BigInt]()
  val ids = new ArrayBuffer[String]()

  private def findIdxFromID(id:String, makeNewID:Boolean=false):Int = {
    for (i <- 0 until ids.size) {
      if (ids(i) == id) return i    // ID found
    }
    // ID not found
    if (makeNewID == true) {
      ids.append(id)
      startTimes.append(0)
      deltas.append(0)
      numSamples.append(0)

      return (ids.size - 1)
    } else {
      -1
    }
  }

  def start(id:String): Unit = {
    val idx = findIdxFromID(id, true)
    startTimes(idx) = System.nanoTime()
  }

  def end(id:String): Unit = {
    val idx = findIdxFromID(id, false)
    if (idx >= 0) {
      val endTime = System.nanoTime()
      val delta = endTime - startTimes(idx)
      deltas(idx) += delta
      numSamples(idx) += 1
    }
  }

  def report(): Unit = {
    println (" ------------------------------ ")
    println (" * Profiler report: ")
    for (i <- 0 until ids.size) {
      println (" [" + i + "] id:" + formatFixedLength(ids(i), 30) + "    \t totaltime:" + deltas(i) + "\t samples:" + numSamples(i) + " \tAvg time/sample:" + deltas(i)/numSamples(i))
    }
    println (" ------------------------------ ")
  }


  def formatFixedLength(in:String, length:Int):String = {
    // There is probably a helper function for this somewhere in the standard library, or an option to formatted(), but I haven't found it yet. :)
    var os:String = in
    for (i <- 0 until (length - in.size)) os += " "
    os
  }

}
