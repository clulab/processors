package org.clulab.processors.hexatagging

import scala.collection.mutable.ArrayBuffer

import org.clulab.struct.HeapElement
import org.clulab.struct.MinHeap
import org.clulab.struct.MinHeapIterator
import org.clulab.utils.Ops.ObjectOps

class Path[L](val score: Float, val sequence: Seq[L]) extends HeapElement {
  def append(elem: L, elemScore: Float): Path[L] = {
    val newScore = score + elemScore
    val newSeq = sequence :+ elem
    new Path[L](newScore, newSeq)
  }
}

object Path {
  def apply[L](): Path[L] = {
    new Path[L](0.0f, Seq[L]())
  }
}

object TopKPaths {
  /**
    * Finds the top K paths in a decoder's greedy lattice
    * Assumes tags are sorted in descending order of scores
    * @return a sequence of paths sorted in descending order of scores
    */
  def findTopK(tags: Array[Array[(String, Float)]], k: Int): Seq[Path[String]] = {
    var pathHeap = new MinHeap(k)
    pathHeap.insert(Path[String]())

    for(i <- tags.indices) {
      val newHeap = new MinHeap(k)

      val it = new MinHeapIterator(pathHeap)
      while(it.hasNext) {
        val p: Path[String] = it.next()
        var stillWorking = true
        // each column in the lattice must be sorted in descending order of scores
        for(j <- tags(i).indices if stillWorking) {
          val newP = p.append(tags(i)(j)._1, tags(i)(j)._2)
          // if insert fails it means the old path + the new elem < the min score already in the heap
          // since columns are sorted in descending order of scores, we can then stop
          stillWorking = newHeap.insert(newP)
        }
      }

      pathHeap = newHeap
    }

    pathHeap.toSortedSeq.map(_.asInstanceOf[Path[String]])
  }
}
