package org.clulab.struct

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

/**
  * An inverted index of the DirectedGraph, so we can efficiently implement enhanced dependencies
  * User: mihais
  * Date: 8/2/17
  */
class DirectedGraphIndex[E](
  val roots:mutable.HashSet[Int],
  val outgoingEdges:Array[mutable.HashSet[(Int, E)]], // from head to modifier
  val edgesByName:mutable.HashMap[E, mutable.HashSet[(Int, Int)]]) { // indexes edges by label

  def this(len:Int) {
    this(
      new mutable.HashSet[Int],
      DirectedGraphIndex.mkOutgoing(len),
      new mutable.HashMap[E, mutable.HashSet[(Int, Int)]]()
    )
  }

  def addEdge(head:Int, modifier:Int, label:E) {
    outgoingEdges(head) += Tuple2(modifier, label)
    val byLabel = edgesByName.getOrElseUpdate(label, new mutable.HashSet[(Int, Int)]())
    byLabel += Tuple2(head, modifier)
  }

  def removeEdge(head:Int, modifier:Int, label:E): Unit = {
    outgoingEdges(head).remove(Tuple2(modifier, label))
    val byLabel = edgesByName.get(label)
    if(byLabel.nonEmpty) {
      byLabel.get.remove(Tuple2(head, modifier))
    }
  }

  def addRoot(index:Int) { roots += index }

  def findByName(label:E): Seq[Edge[E]] = {
    val edges = new ListBuffer[Edge[E]]
    edgesByName.get(label).foreach(ses =>
      for(se <- ses) {
        edges += Edge(se._1, se._2, label)
      }
    )
    edges
  }

  def findByHeadAndName(head:Int, label:E): Seq[Edge[E]] = {
    findByName(label).filter(_.source == head)
  }

  def findByModifierAndName(modifier:Int, label:E): Seq[Edge[E]] = {
    findByName(label).filter(_.destination == modifier)
  }

  def findByHeadAndPattern(head:Int, pattern:Regex): Seq[Edge[E]] = {
    val edges = new ListBuffer[Edge[E]]
    for(e <- outgoingEdges(head).toList) {
      if(pattern.findFirstMatchIn(e._2.toString).nonEmpty) {
        edges += new Edge[E](head, e._1, e._2)
      }
    }
    edges
  }

  def toDirectedGraph: DirectedGraph[E] = {
    val edges = new ListBuffer[Edge[E]]
    for(head <- outgoingEdges.indices) {
      for(ml <- outgoingEdges(head)) {
        val e = new Edge[E](head, ml._1, ml._2)
        edges += e
      }
    }
    new DirectedGraph[E](edges.toList, roots.toSet)
  }
}

object DirectedGraphIndex {
  private def mkOutgoing[E](len:Int): Array[mutable.HashSet[(Int, E)]] = {
    val outgoing = new Array[mutable.HashSet[(Int, E)]](len)
    for(i <- outgoing.indices)
      outgoing(i) = new mutable.HashSet[(Int, E)]()
    outgoing
  }
}
