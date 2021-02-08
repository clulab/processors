package org.clulab.processors

import org.clulab.struct.DirectedGraph

/** Makes sure that CluProcessor produces dependency graphs of correct sizes */
class TestDepGraphSizes extends FatdynetTest {

  "CluProcessor" should "produce dependency graphs that have the same size as the sentence" in {
    // Document 3
    // val text = "Raise fertility on \n\n"
    // Document 11
    val text = "In order to reduce the risk of civil conflict in this context , it is necessary to increase the costs of participating in \n\n"
    text.length > 0 should be(true)

    val doc = proc.annotate(text)

    for(sent <- doc.sentences) {
      val size = sent.size

      val d1 = sent.universalBasicDependencies.get
      checkGraph(sent, d1, "universalBasicDependencies")
      d1.size should be (size)

      val d2 = sent.universalEnhancedDependencies.get
      checkGraph(sent, d2, "universalEnhancedDependencies")
      d2.size should be (size)

      val d3 = sent.semanticRoles.get
      checkGraph(sent, d3, "semanticRoles")
      d3.size should be (size)

      val d4 = sent.enhancedSemanticRoles.get
      checkGraph(sent, d4, "enhancedSemanticRoles")
      d4.size should be (size)
    }
  }

  def checkGraph(sent: Sentence, graph: DirectedGraph[String], name: String): Unit = {
    if(graph.size != sent.size) {
      println(s"The graph $name for this sentence is irregular: ${sent.size} tokens vs. ${graph.size} graph size!")
      println("Tokens: " + sent.words.mkString(", "))
      println(s"Graph $name:")
      println(graph)
    }
  }
}
