package org.clulab.struct

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TestDirectedGraph extends FlatSpec with Matchers {

  behavior of "DirectedGraph"

  def newEdge(source: Int, destination: Int): Edge[String] = {
    val relation = s""

    Edge(source, destination, relation)
  }

  it should "process a loop with a source" in {
    val edges = List(
      newEdge(0, 1),
      newEdge(1, 2),
      newEdge(2, 1)
    )
    val directedGraph = new DirectedGraph(edges)

    directedGraph.size should be (3)
    directedGraph.roots should be (Set(0))
  }

  it should "process a loop without a source" in {
    val edges = List(
      newEdge(0, 1),
      newEdge(1, 2),
      newEdge(2, 0)
    )
    val directedGraph = new DirectedGraph(edges)

    directedGraph.size should be (3)
    directedGraph.roots should be (Set(0))
  }

  it should "add lone roots" in {
    val edges = List(
      newEdge(1, 2),
      newEdge(2, 3),
      newEdge(3, 1)
    )
    val directedGraph = new DirectedGraph(edges, Some(5))

    directedGraph.size should be (5)
    directedGraph.roots should be (Set(0, 1, 4))
  }

  it should "handle disjoint loops" in {
    val edges = List(
      newEdge(0, 1),
      newEdge(1, 2),
      newEdge(2, 0),

      newEdge(3, 4),
      newEdge(4, 5),
      newEdge(5, 3)
    )
    val directedGraph = new DirectedGraph(edges)

    directedGraph.size should be (6)
    directedGraph.roots should be (Set(0, 3))
  }

  it should "not use a leading destination as a root" in {
    val edges = List(
      newEdge(1, 0),
      newEdge(1, 2),
      newEdge(2, 1)
    )
    val directedGraph = new DirectedGraph(edges)

    directedGraph.size should be (3)
    directedGraph.roots should be (Set(1))
  }

  it should "be empty" in {
    val edges = List.empty[Edge[String]]

    val directedGraph = new DirectedGraph(edges)

    directedGraph.size should be (0)
    directedGraph.roots should be (Set())
  }
}
