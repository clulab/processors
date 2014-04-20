package edu.arizona.sista.processors.struct

import collection.mutable.{ListBuffer, ArrayBuffer}

/**
 * A generic graph where the nodes have Int identifiers and edges have type E
 * The node ids are offsets in an array thus they must start at 0
 * This class is designed to be as immutable as possible, thus no operations to modify it are provided
 * User: mihais
 * Date: 3/5/13
 */
class DirectedGraph[E](edges:List[(Int, Int, E)], val roots:collection.immutable.Set[Int]) extends Serializable {
  val outgoingEdges:Array[Array[(Int, E)]] = mkOutgoing(edges)
  val incomingEdges:Array[Array[(Int, E)]] = mkIncoming(edges)

  private def computeSize(edges:List[(Int, Int, E)]):Int = {
    var size = 0
    for (e <- edges) {
      size = math.max(e._1 + 1, size)
      size = math.max(e._2 + 1, size)
    }
    size
  }

  private def mkOutgoing(edges:List[(Int, Int, E)]): Array[Array[(Int, E)]] = {
    //println("EDGES:")
    //for(e <- edges) println(e._1 + " " + e._2 + " " + e._3)
    val size = computeSize(edges)
    //println("size = " + size)
    val nodes = new Array[ArrayBuffer[(Int, E)]](size)
    var offset = 0
    while(offset < nodes.length) {
      nodes(offset) = new ArrayBuffer[(Int, E)]
      offset += 1
    }

    for (edge <- edges) {
      //println("storing edge: " + edge)
      nodes(edge._1).+=((edge._2, edge._3))
    }

    val outgoing = new Array[Array[(Int, E)]](size)
    offset = 0
    while(offset < nodes.length) {
      outgoing(offset) = nodes(offset).sortBy(e => e._1).toArray
      offset += 1
    }

    outgoing
  }

  private def mkIncoming(edges:List[(Int, Int, E)]): Array[Array[(Int, E)]] = {
    val size = computeSize(edges)
    //println("size = " + size)
    val nodes = new Array[ArrayBuffer[(Int, E)]](size)
    var offset = 0
    while(offset < nodes.length) {
      nodes(offset) = new ArrayBuffer[(Int, E)]
      offset += 1
    }

    for (edge <- edges) {
      //println("storing edge: " + edge)
      nodes(edge._2).+=((edge._1, edge._3))
    }

    val incoming = new Array[Array[(Int, E)]](size)
    offset = 0
    while(offset < nodes.length) {
      incoming(offset) = nodes(offset).sortBy(e => e._1).toArray
      offset += 1
    }

    incoming
  }

  def size:Int = outgoingEdges.length

  def getOutgoingEdges(node:Int): Array[(Int, E)] = outgoingEdges(node)
  def getIncomingEdges(node:Int): Array[(Int, E)] = incomingEdges(node)

  def hasEdge(from:Int, to:Int, v:E):Boolean = {
    val fromEdges = outgoingEdges(from)
    var offset = 0
    while(offset < fromEdges.length) {
      //println("checking edge: " + from + " " + fromEdges(offset)._1 + " " + fromEdges(offset)._2 + " against " + to + " " + v)
      if (fromEdges(offset)._1 == to && fromEdges(offset)._2 == v) {
        //println("\t\tTRUE")
        return true
      }
      offset += 1
    }
    false
  }

  override def toString:String = {
    val os = new StringBuilder

    os.append("roots: " + roots.mkString(sep = ",") + "\n")
    os.append("outgoing:\n")
    var n = 0
    while(n < size) {
      os.append("\t" + n + ":")
      for (e <- outgoingEdges(n)) {
        os.append(" " + e)
      }
      os.append("\n")
      n += 1
    }
    os.append("incoming:\n")
    n = 0
    while(n < size) {
      os.append("\t" + n + ":")
      for (e <- incomingEdges(n)) {
        os.append(" " + e)
      }
      os.append("\n")
      n += 1
    }

    os.toString()
  }
}

class DirectedGraphEdgeIterator[E](val graph:DirectedGraph[E]) extends Iterator[(Int, Int, E)] {
  var node = findNextNodeWithEdges(0)
  var nodeEdgeOffset = 0

  def findNextNodeWithEdges(start:Int):Int = {
    var n = start
    while (n < graph.size) {
      if (graph.getOutgoingEdges(n).length > 0)
        return n
      n += 1
    }
    return graph.size
  }

  def hasNext:Boolean = {
    return (node < graph.size)
  }

  def next:(Int, Int, E) = {
    val edge = graph.getOutgoingEdges(node)(nodeEdgeOffset)
    val from = node
    if (nodeEdgeOffset < graph.getOutgoingEdges(node).length - 1) {
      nodeEdgeOffset += 1
    } else {
      node = findNextNodeWithEdges(node + 1)
      nodeEdgeOffset = 0
    }
    return (from, edge._1, edge._2)
  }
}
