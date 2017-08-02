package org.clulab.struct

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * An inverted index of the DirectedGraph, so we can efficiently implement enhanced dependencies
  * User: mihais
  * Date: 8/2/17
  */
class DirectedGraphIndex[E](
   val roots:mutable.HashSet[Int],
   val outgoingEdges:Array[ArrayBuffer[(Int, E)]], // from head to modifier
   val edgesByName:mutable.HashMap[E, ArrayBuffer[(Int, Int)]]) { // indexes edges by label

  def this(len:Int) {
    this(
      new mutable.HashSet[Int],
      new Array[ArrayBuffer[(Int, E)]](len),
      new mutable.HashMap[E, ArrayBuffer[(Int, Int)]]()
    )
  }

  def addEdge(head:Int, modifier:Int, label:E): Unit = {
    outgoingEdges(head) += new Tuple2(modifier, label)
    val byLabel = edgesByName.getOrElseUpdate(label, new ArrayBuffer[(Int, Int)]())
    byLabel += new Tuple2(head, modifier)
  }

  def addRoot(index:Int) { roots += index }

  def toDirectedGraph: DirectedGraph[E] = {
    
  }
}
