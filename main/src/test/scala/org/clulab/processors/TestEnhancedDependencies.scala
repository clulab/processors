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

  "CluProcessor" should "parse some basic sentences correctly" in {
    var doc = proc.annotate("Ras1 is associated with cancer.")
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 0, "nsubjpass") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 1, "auxpass") should be(true)

    doc = proc.annotate("Ras1 has phosphorylated Mek2.")
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 0, "nsubj") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 1, "aux") should be(true)

    doc = proc.annotate("John has travelled to China.")
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 0, "nsubj") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 1, "aux") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 3, "prep") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(3, 4, "pobj") should be(true)

    doc = proc.annotate("John is traveling to China.")
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 0, "nsubj") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 1, "aux") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 3, "prep") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(3, 4, "pobj") should be(true)
    
    doc = proc.annotate("John Doe will travel to China.")
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(3, 1, "nsubj") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(1, 0, "nn") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(3, 2, "aux") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(3, 4, "prep") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(4, 5, "pobj") should be(true)

    doc = proc.annotate("Mary has been reading a book")
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(3, 0, "nsubj") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(3, 1, "aux") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(3, 2, "aux") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(3, 5, "dobj") should be(true)

    doc = proc.annotate("Mary is reading a book.")
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 0, "nsubj") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 1, "aux") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 4, "dobj") should be(true)

    doc = proc.annotate("Paul and Mary are reading a book.")
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(4, 0, "nsubj") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(0, 2, "conj") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(4, 3, "aux") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(4, 6, "dobj") should be(true)
  }

  it should "collapse prepositions" in {
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
