package org.clulab.struct

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.hashing.MurmurHash3._


/**
 * A generic graph where the nodes have Int identifiers and edges have type E
 * The node ids are offsets in an array thus they must start at 0
 * This class is designed to be as immutable as possible, thus no operations to modify it are provided
 * Each edge in edges stores (head, modifier, label)
 * User: mihais
 * Date: 3/5/13
 */
case class DirectedGraph[E](edges: List[Edge[E]], roots: collection.immutable.Set[Int]) extends Serializable {
  val outgoingEdges: Array[Array[(Int, E)]] = mkOutgoing(edges, roots)
  val incomingEdges: Array[Array[(Int, E)]] = mkIncoming(edges, roots)

  val allEdges: List[(Int, Int, E)] = edges.map(e => (e.source, e.destination, e.relation))

  /**
    * Used to compare DirectedGraphs.
    * @return a hash (Int) based on the [[edges]] and [[roots]]
    */
  def equivalenceHash: Int = {
    val stringCode = "org.clulab.struct.DirectedGraph"
    // the seed (not counted in the length of finalizeHash)
    // decided to use the class name
    val h0 = stringHash(stringCode)
    val h1 = mix(h0, edges.hashCode)
    val h2 = mix(h1, roots.hashCode)
    finalizeHash(h2, 2)
  }

  private def computeSize(edges:List[Edge[_]], roots: collection.immutable.Set[Int]):Int = {
    var size = 0
    for (e <- edges) {
      size = math.max(e.source + 1, size)
      size = math.max(e.destination + 1, size)
    }
    for(r <- roots) {
      size = math.max(r + 1, size)
    }
    size
  }

  // (src, dest, rel) <- allEdges

  private def mkOutgoing(edges:List[Edge[E]], roots: collection.immutable.Set[Int]): Array[Array[(Int, E)]] = {
    //println("EDGES:")
    //for(e <- edges) println(e._1 + " " + e._2 + " " + e._3)
    val size = computeSize(edges, roots)
    //println("size = " + size)
    val nodes = new Array[ArrayBuffer[(Int, E)]](size)
    var offset = 0
    while(offset < nodes.length) {
      nodes(offset) = new ArrayBuffer[(Int, E)]
      offset += 1
    }

    for (edge <- edges) {
      //logger.debug("storing edge: " + edge)
      nodes(edge.source).+=((edge.destination, edge.relation))
    }

    val outgoing = new Array[Array[(Int, E)]](size)
    offset = 0
    while(offset < nodes.length) {
      outgoing(offset) = nodes(offset).sortBy(e => e._1).toArray
      offset += 1
    }

    outgoing
  }

  private def mkIncoming(edges:List[Edge[E]], roots: collection.immutable.Set[Int]): Array[Array[(Int, E)]] = {
    val size = computeSize(edges, roots)
    //println("size = " + size)
    val nodes = new Array[ArrayBuffer[(Int, E)]](size)
    var offset = 0
    while(offset < nodes.length) {
      nodes(offset) = new ArrayBuffer[(Int, E)]
      offset += 1
    }

    for (edge <- edges) {
      //logger.debug("storing edge: " + edge)
      nodes(edge.destination).+=((edge.source, edge.relation))
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

  // gets edges between nodes, optionally ignoring direction
  def getEdges(n1: Int, n2: Int, ignoreDirection: Boolean = false): Seq[(Int, Int, E)] = allEdges.filter {
        case (`n1`, `n2`, _) => true
        case (`n2`, `n1`, _) if ignoreDirection => true
        case _ => false
      }

  // Gets a single path represented as a sequence in which each element
  // is a sequence of edges connecting two tokens, and returns a sequence in which
  // each element is a sequence of edges forming a path.
  // In other words, it returns all possible paths given the edges connecting the nodes of interest.
  def mkEdgePaths(edges: Seq[Seq[(Int, Int, E)]]): Seq[Seq[(Int, Int, E)]] = edges match {
    case Nil => Seq(Nil)
    case Seq(first, rest @ _*) => for {
      i <- first
      j <- mkEdgePaths(rest)
    } yield i +: j
  }

  // returns the shortest path between two nodes as a sequence of nodes
  // each pair of nodes is guaranteed to have at least one edge, maybe several
  def shortestPath(start: Int, end: Int, ignoreDirection: Boolean = false): Seq[Int] = {
    def neighbors(node: Int): Seq[Int] = {
      val edges = outgoingEdges(node) ++ (if (ignoreDirection) incomingEdges(node) else Nil)
      edges.map(_._1).distinct
    }

    // build table of pointers to previous node in shortest path to the source
    @annotation.tailrec
    def mkPrev(nodes: Set[Int], dist: Map[Int, Double], prev: Map[Int, Int]): Map[Int, Int] =
      if (nodes.isEmpty) prev
      else {
        val u = nodes minBy dist
        val d = dist(u) + 1  // all edges have a cost of 1
        val newDistPrev = for {
          v <- neighbors(u)
          if nodes.contains(v) && d < dist(v)
        } yield (v -> d, v -> u)
        val (newDist, newPrev) = newDistPrev.unzip
        mkPrev(nodes - u, dist ++ newDist, prev ++ newPrev)
      }

    // build path from source to node
    @annotation.tailrec
    def mkPath(node: Int, prev: Map[Int, Int], path: Seq[Int]): Seq[Int] =
      if (prev contains node) mkPath(prev(node), prev, node +: path) else path

    val nodes = (0 until size).toSet
    val dist = Map(start -> 0.0) withDefaultValue Double.PositiveInfinity
    val prev = Map(start -> -1)  // start has no previous node
    mkPath(end, mkPrev(nodes, dist, prev), Nil)
  }

  // the edge tuple is (head:Int, dependent:Int, label:E, direction:String)
  def shortestPathEdges(start: Int, end: Int, ignoreDirection: Boolean = false): Seq[Seq[(Int, Int, E, String)]] = {
    // get sequence of nodes in the shortest path
    val nodesPath = shortestPath(start, end, ignoreDirection)
    // make pairs of nodes in the shortest path
    val pairs = nodesPath.sliding(2).toList
    // get edges for each pair
    val edges = for (Seq(n1, n2) <- pairs) yield getEdges(n1, n2, ignoreDirection)
    // return sequence of paths, where each path is a sequence of edges
    for (edgePath <- mkEdgePaths(edges)) yield {
      for ((Seq(n1, n2), edge) <- pairs zip edgePath) yield edge match {
        case (`n1`, `n2`, dep) => (n1, n2, dep, ">")
        case (`n2`, `n1`, dep) => (n2, n1, dep, "<")
        case _ => sys.error("unrecognized edge")
      }
    }
  }

  def containsCycles():Boolean = {
    for(i <- 0 until size) {
      val traversed = new mutable.HashSet[Int]
      if(hasCycle(i, traversed)) {
        return true
      }
    }
    false
  }

  private def hasCycle(current:Int, traversed:mutable.HashSet[Int]):Boolean = {
    if(traversed.contains(current)) {
      // println(s"Found cycle on offset $current!")
      return true
    } else if(incomingEdges(current).nonEmpty) {
      // assumption: each node has a single head (stored in incoming)
      traversed += current
      return hasCycle(incomingEdges(current)(0)._1, traversed)
    }
    false
  }

  def toDirectedGraphIndex: DirectedGraphIndex[E] = {
    val dgi = new DirectedGraphIndex[E](size)
    roots.foreach(dgi.addRoot(_))
    allEdges.foreach(e => dgi.addEdge(e._1, e._2, e._3))
    dgi
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
    graph.size
  }

  def hasNext:Boolean = node < graph.size

  def next:(Int, Int, E) = {
    val edge = graph.getOutgoingEdges(node)(nodeEdgeOffset)
    val from = node
    if (nodeEdgeOffset < graph.getOutgoingEdges(node).length - 1) {
      nodeEdgeOffset += 1
    } else {
      node = findNextNodeWithEdges(node + 1)
      nodeEdgeOffset = 0
    }
    (from, edge._1, edge._2)
  }
}

object DirectedGraph {

  def triplesToEdges[E](triples: List[(Int, Int, E)]): List[Edge[E]] = for {
    triple <- triples
  } yield Edge[E](source = triple._1, destination = triple._2, relation = triple._3)

  def edgesToTriples[E](edges: Seq[Edge[E]]): Seq[(Int, Int, E)] = for {
    edge <- edges
  } yield (edge.source, edge.destination, edge.relation)

  /**
   * Constructs a graph from Stanford dependencies
   * Note: Stanford indices start at 1, so we will decrement all indices by 1
   */
  def mkGraph(dependencies: Array[String]): DirectedGraph[String] = {
    val edges = new ListBuffer[Edge[String]]
    val roots = new mutable.HashSet[Int]()
    for (depLine <- dependencies) {
      parseDep(depLine).foreach(dep => {
        edges += Edge(dep._1, dep._2, dep._3)
        if (dep._1 == -1) roots.add(dep._2)
      })
    }
    DirectedGraph[String](edges.toList, roots.toSet)
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

case class Edge[E](
  source: Int,
  destination: Int,
  relation: E
)
