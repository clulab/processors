package edu.arizona.sista.struct

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}

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

  // returns path from `start` to `end` as a string
  def path(start: Int, end: Int): String = {
    val nodes = shortestPath(start, end)
    val pairs = (1 until nodes.length) map (i => (nodes(i-1), nodes(i)))
    val steps = for ((n1, n2) <- pairs) yield edge(n1, n2) match {
      case Some((`n1`, `n2`, dep)) => s">$dep"
      case Some((`n2`, `n1`, dep)) => s"<$dep"
      case _ => sys.error("path error")
    }
    steps.mkString(" ")
  }

  // gets edge between nodes, ignoring direction
  private def edge(n1: Int, n2: Int) = edges find {
    case (`n1`, `n2`, _) => true
    case (`n2`, `n1`, _) => true
    case _ => false
  }

  private def shortestPath(start: Int, end: Int) = {
    def neighborsFor(node: Int): Seq[Int] =
      (outgoingEdges(node) ++ incomingEdges(node)).map(_._1).distinct

    // build table of pointers to previous node in shortest path to the source
    @annotation.tailrec
    def mkPrev(rest: Set[Int], dist: Map[Int, Double], prev: Map[Int, Int]): Map[Int, Int] = {
      if (rest.isEmpty) prev
      else {
        val u = rest minBy dist
        val (newDist, newPrev) = (neighborsFor(u) filter rest.contains flatMap { v =>
          val d = dist(u) + 1  // all edges have a cost of 1
          if (d < dist(v)) Some(((v -> d), (v -> u))) else None
        }).unzip
        mkPrev(rest - u, dist ++ newDist, prev ++ newPrev)
      }
    }

    // build path from source to node
    @annotation.tailrec
    def mkPath(node: Int, prev: Map[Int, Int], path: Seq[Int]): Seq[Int] = {
      if (prev contains node) mkPath(prev(node), prev, node +: path)
      else node +: path
    }

    val nodes = (0 until size).toSet
    val dist = Map(start -> 0.0) withDefaultValue Double.PositiveInfinity
    val prev = mkPrev(nodes, dist, Map.empty)
    mkPath(end, prev, Nil)
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

object DirectedGraph {
  /**
   * Constructs a graph from Stanford dependencies
   * Note: Stanford indices start at 1, so we will decrement all indices by 1
   */
  def mkGraph(dependencies: Array[String]): DirectedGraph[String] = {
    val edges = new ListBuffer[(Int, Int, String)]
    val roots = new mutable.HashSet[Int]()
    for (depLine <- dependencies) {
      parseDep(depLine).foreach(dep => {
        edges += dep
        if (dep._1 == -1) roots.add(dep._2)
      })
    }
    new DirectedGraph[String](edges.toList, roots.toSet)
  }

  /** Parses a line of the form "nsubjpass(expressed-15, CDK6-13)" into a tuple(14, 12, nsubjpass) */
  def parseDep(line: String): Option[(Int, Int, String)] = {
    val endLabel = line.indexOf("(")
    assert(endLabel > 0)
    val label = line.substring(0, endLabel)
    println("LABEL = " + label)
    assert(line.last == ')')
    val hm = line.substring(endLabel + 1, line.length - 1)
    println("HM = " + hm)
    val sep = hm.indexOf(", ")
    assert(sep > 0)
    val h = hm.substring(0, sep)
    val m = hm.substring(sep + 2)
    val hv = cleanNumber(h.substring(h.lastIndexOf("-") + 1)).toInt // remove appostrophies which indicate duplicated nodes, see http://nlp.stanford.edu/software/dependencies_manual.pdf#page=16
    val mv = cleanNumber(m.substring(m.lastIndexOf("-") + 1)).toInt

    if (hv == mv) {
      // this simply indicates a duplicated node; not a real dependency
      None
    } else {
      Some(hv, mv, label)
    }
  }

  def cleanNumber(v: String): String = {
    val b = new mutable.StringBuilder()
    for (i <- 0 until v.length) {
      if (Character.isDigit(v.charAt(i)))
        b.append(v.charAt(i))
    }
    b.toString()
  }
}
