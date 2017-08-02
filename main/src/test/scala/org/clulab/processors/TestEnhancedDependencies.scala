package org.clulab.processors

import org.clulab.processors.clu.CluProcessor
import org.scalatest.{FlatSpec, Matchers}

/**
  * Unit tests for our own implementation of Stanford's collapsed (enhanced) dependencies
  * User: mihais
  * Date: 8/2/17
  */
class TestEnhancedDependencies extends FlatSpec with Matchers {
  val proc = new CluProcessor

  "CluProcessor" should "collapse prepositions" in {
    val doc = proc.annotate("Mary gave a book to Jane")
    doc.sentences.head.stanfordCollapsedDependencies.get.hasEdge(1, 5, "prep_to") should be(true)
    doc.sentences.head.stanfordCollapsedDependencies.get.hasEdge(1, 4, "prep") should be(false)
    doc.sentences.head.stanfordCollapsedDependencies.get.hasEdge(4, 5, "pobj") should be(false)
  }

  it should "capture raised subjects" in {
    val doc = proc.annotate("Mary wants to buy a book")
    doc.sentences.head.stanfordCollapsedDependencies.get.hasEdge(3, 0, "nsubj") should be(true)
  }
}
