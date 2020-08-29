package org.clulab.processors

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.scalatest.{FlatSpec, Matchers}

class TestEnhancedSemanticRoles extends FlatSpec with Matchers {
  val proc = {
    Utils.initializeDyNet()
    new CluProcessor()
  }

  it should "collapse prepositions" in {
    val doc = proc.annotate("He gave the book to Jane.")
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(1, 3, "A1") should be(true)
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(1, 5, "Ax_to") should be(true)
    doc.sentences.head.enhancedSemanticRoles.get.hasEdge(1, 4, "Ax") should be(false)
  }
}
