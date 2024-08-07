package org.clulab.struct

import org.clulab.scala.WrappedListBuffer._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

/**
  * An inverted index of the DirectedGraph, so we can efficiently implement enhanced dependencies
  * User: mihais
  * Date: 8/2/17
  */
class DirectedGraphIndex[E](
  val size: Int,
  val outgoingEdges:Array[mutable.HashSet[(Int, E)]], // from head to modifier
  val incomingEdges:Array[mutable.HashSet[(Int, E)]], // from modifier to head
  val edgesByName:mutable.HashMap[E, mutable.HashSet[(Int, Int)]]) { // indexes edges by label

  def this(sentenceLength:Int) = {
    this(sentenceLength,
      DirectedGraphIndex.mkOutgoing[E](sentenceLength),
      DirectedGraphIndex.mkIncoming[E](sentenceLength),
      new mutable.HashMap[E, mutable.HashSet[(Int, Int)]]()
    )
  }

  def addEdge(head:Int, modifier:Int, label:E): Unit = {
    outgoingEdges(head) += ((modifier, label))
    incomingEdges(modifier) += ((head, label))
    val byLabel = edgesByName.getOrElseUpdate(label, new mutable.HashSet[(Int, Int)]())
    byLabel += ((head, modifier))
  }

  def removeEdge(head:Int, modifier:Int, label:E): Unit = {
    outgoingEdges(head).remove((modifier, label))
    incomingEdges(modifier).remove((head, label))
    val byLabel = edgesByName.get(label)
    if(byLabel.nonEmpty) {
      byLabel.get.remove((head, modifier))
    }
  }

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

  def findByModifier(modifier:Int): Seq[Edge[E]] = {
    val edges = new ListBuffer[Edge[E]]
    for(e <- incomingEdges(modifier)) {
      edges += new Edge[E](e._1, modifier, e._2)
    }
    edges
  }

  def findByHead(head:Int): Seq[Edge[E]] = {
    val edges = new ListBuffer[Edge[E]]
    for(e <- outgoingEdges(head)) {
      edges += new Edge[E](head, e._1, e._2)
    }
    edges
  }

  def findByHeadAndPattern(head:Int, pattern:Regex): Seq[Edge[E]] = {
    val edges = new ListBuffer[Edge[E]]
    if(head < outgoingEdges.length) {
      for (e <- outgoingEdges(head).toList) {
        if (pattern.findFirstMatchIn(e._2.toString).nonEmpty) {
          edges += new Edge[E](head, e._1, e._2)
        }
      }
    }
    edges
  }

  def findByModifierAndPattern(modifier:Int, pattern:Regex): Seq[Edge[E]] = {
    val edges = new ListBuffer[Edge[E]]
    if(modifier < incomingEdges.length) {
      for (e <- incomingEdges(modifier).toList) {
        if (pattern.findFirstMatchIn(e._2.toString).nonEmpty) {
          edges += new Edge[E](e._1, modifier, e._2)
        }
      }
    }
    edges
  }

  def mkEdges(): List[Edge[E]] = {
    val edges = new ListBuffer[Edge[E]]

    for (head <- outgoingEdges.indices) {
      for (ml <- outgoingEdges(head)) {
        val e = new Edge[E](head, ml._1, ml._2)
        edges += e
      }
    }
    edges.toList
  }

  def toDirectedGraph(preferredSize: Option[Int] = None): DirectedGraph[E] = {
    val edges = mkEdges()

    new DirectedGraph[E](edges, preferredSize)
  }
}

object DirectedGraphIndex {
  private def mkOutgoing[E](len:Int): Array[mutable.HashSet[(Int, E)]] = {
    val outgoing = new Array[mutable.HashSet[(Int, E)]](len)
    for(i <- outgoing.indices)
      outgoing(i) = new mutable.HashSet[(Int, E)]()
    outgoing
  }

  private def mkIncoming[E](len:Int): Array[mutable.HashSet[(Int, E)]] = {
    val incoming = new Array[mutable.HashSet[(Int, E)]](len)
    for(i <- incoming.indices)
      incoming(i) = new mutable.HashSet[(Int, E)]()
    incoming
  }
}
