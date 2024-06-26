package org.clulab.processors.hexatagging

import scala.collection.mutable.ArrayBuffer

import org.clulab.struct.HeapElement
import org.clulab.struct.MinHeap
import org.clulab.struct.MinHeapIterator
import org.clulab.utils.Ops.ObjectOps
import org.clulab.utils.MathUtils

class Path[L](val score: Float, val sequence: Seq[L]) extends HeapElement {
  def append(elem: L, elemScore: Float): Path[L] = {
    val newScore = score + elemScore
    val newSeq = sequence :+ elem
    new Path[L](newScore, newSeq)
  }

  override def toString(): String = {
    s"Path($score): [${sequence.mkString(", ")}]"
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
  def findTopK(tags: Array[Array[(String, Float)]], k: Int, applySoftmax: Boolean = true): Seq[Path[String]] = {
    //println(s"Inside findTopK ${tags.length} x ${tags(0).length}, k = $k")
    var pathHeap = new MinHeap(k)
    pathHeap.insert(Path[String]())

    for(i <- tags.indices) {
      // logits for token i
      val scores = tags(i).map(_._2)
      // probs
      val probs = 
        if(applySoftmax) MathUtils.denseSoftmaxFloat(scores)
        else scores
      // log probs
      val logProbs = probs.map(Math.log(_).toFloat)

      val newHeap = new MinHeap(k)

      val it = new MinHeapIterator(pathHeap)
      while(it.hasNext) {
        val p = it.next().asInstanceOf[Path[String]]
        var stillWorking = true
        // each column in the lattice must be sorted in descending order of scores
        for(j <- 0 until tags(i).length if stillWorking) {
          val newP = p.append(tags(i)(j)._1, logProbs(j))
          //println(s"newP = $newP")
          // if insert fails it means the old path + the new elem < the min score already in the heap
          // since columns are sorted in descending order of scores, we can stop here
          stillWorking = newHeap.insert(newP)
        }
      }

      pathHeap = newHeap
    }

    val paths = pathHeap.toSortedSeq.map(_.asInstanceOf[Path[String]])
    //println(paths.head)
    //println(paths.size)
    paths
  }
}
