package org.clulab.processors.hexatagging

import scala.collection.mutable.ArrayBuffer

class Path[L](var score: Float, val sequence: ArrayBuffer[L])

object Path {
  def apply[L](): Path[L] = {
    new Path[L](0.0f, new ArrayBuffer[L]())
  }
}

object TopKPaths {
  /**
    * Finds the top K paths in a decoder's greedy lattice
    * Assumes tags ae sorted in descending order of scores
    */
  def findTopK(tags: Array[Array[(String, Float)]], k: Int): Seq[Path[String]] = {
    // TODO
    val p = Path[String]()
    for(i <- tags.indices) {
      p.score += tags(i).head._2
      p.sequence += tags(i).head._1
    }
    Seq(p)
  }
}
