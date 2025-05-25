package org.clulab.utils

import org.clulab.processors.Sentence
import org.clulab.struct.GraphMap.UNIVERSAL_BASIC
import org.clulab.struct.{DirectedGraph, Edge, Interval}

class TestFindHeads extends Test {

  def newSentence(words: Seq[String], directedGraph: DirectedGraph[String]): Sentence = {
    val startOffsets = Seq(0) // unused
    val   endOffsets = Seq(0) // unused
    val sentence = new Sentence(
      words, startOffsets, endOffsets, words,
      tags = Some(words)
    )

    sentence.graphs(UNIVERSAL_BASIC) = directedGraph
    sentence
  }

  behavior of "DirectedGraph"

  it should "not suffer from issue #134" in {
    val edges = List(
      Edge( 1,  0, ""),
      Edge( 1,  4, ""),
      Edge( 1,  7, ""),
      Edge( 1, 77, ""),

      Edge( 4,  2, ""),
      Edge( 4,  3, ""),

      Edge( 5,  5, ""),
      Edge( 5,  5, ""),
      Edge( 5,  5, ""),
      Edge( 5,  8, ""),
      Edge( 5, 12, ""),
      Edge( 5, 13, ""),
      Edge( 5, 17, ""),
      Edge( 5, 19, ""),
      Edge( 5, 22, ""),
      Edge( 5, 23, ""),
      Edge( 5, 25, ""),

      Edge( 7,  6, ""),

      Edge(12, 10, ""),
      Edge(12, 11, ""),

      Edge(17, 15, ""),
      Edge(17, 16, ""),

      Edge(22, 21, ""),

      Edge(25, 30, ""),
      Edge(25, 32, ""),

      Edge(28, 27, ""),

      Edge(30, 28, ""),
      Edge(30, 29, ""),
      Edge(30, 32, ""),

      Edge(32, 33, ""),

      Edge(33, 36, ""),

      Edge(36, 34, ""),
      Edge(36, 35, ""),
      Edge(36, 38, ""),
      Edge(36, 45, ""),

      Edge(38, 37, ""),
      Edge(38, 42, ""),

      Edge(42, 40, ""),
      Edge(42, 41, ""),

      Edge(45, 44, ""),
      Edge(45, 46, ""),
      Edge(45, 50, ""),
      Edge(45, 54, ""),
      Edge(45, 65, ""),

      Edge(50, 49, ""),
      Edge(50, 51, ""),
      Edge(50, 54, ""),
      Edge(50, 62, ""),
      Edge(50, 65, ""),

      Edge(54, 52, ""),
      Edge(54, 53, ""),
      Edge(54, 55, ""),

      Edge(55, 57, ""),

      Edge(57, 61, ""),

      Edge(61, 59, ""),
      Edge(61, 60, ""),

      Edge(65, 64, ""),
      Edge(65, 67, ""),

      Edge(67, 66, ""),
      Edge(67, 68, ""),
      Edge(67, 69, ""),
      Edge(67, 70, ""),
      Edge(67, 71, ""),
      Edge(67, 72, ""),
      Edge(67, 75, ""),
      Edge(67, 76, ""),

      Edge(75, 73, ""),
      Edge(75, 74, "")
    )
    val len: Int = 78
    val directedGraph = DirectedGraph(edges)
    val tokenInterval = Interval(0, len)
    val words = 1.to(len).map { index => s"word$index" }
    val sentence = newSentence(words, directedGraph)
    val heads = DependencyUtils.findHeadsStrict(tokenInterval, sentence)

    println(heads)
  }
}
