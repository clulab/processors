package org.clulab.processors

import org.clulab.processors.clu.CluProcessor
import org.scalatest.{FlatSpec, Matchers}

class TestUniversalEnhancedDependencies extends FlatSpec with Matchers {
  val proc = new CluProcessor()

  "CluProcessor" should "parse some basic sentences correctly" in {
    var doc = proc.annotate("Ras1 is associated with cancer.")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 0, "nsubjpass") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 1, "auxpass") should be(true)

    doc = proc.annotate("Ras1 has phosphorylated Mek2.")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 0, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 1, "aux") should be(true)

    doc = proc.annotate("John has travelled to China.")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 0, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 1, "aux") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 4, "nmod") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 3, "case") should be(true)

    doc = proc.annotate("John is traveling to China.")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 0, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 1, "aux") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 4, "nmod") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 3, "case") should be(true)

    doc = proc.annotate("John Doe will travel to China.")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 1, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(1, 0, "compound") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 2, "aux") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 5, "nmod") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(5, 4, "case") should be(true)

    doc = proc.annotate("Mary has been reading a book")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 0, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 1, "aux") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 2, "aux") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 5, "dobj") should be(true)

    doc = proc.annotate("Mary is reading a book.")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 0, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 1, "aux") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 4, "dobj") should be(true)

    doc = proc.annotate("Paul and Mary are reading a book.")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 0, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(0, 2, "conj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 3, "aux") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 6, "dobj") should be(true)
  }

  it should "collapse prepositions" in {
    val doc = proc.annotate("Mary gave a book to Jane")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(1, 5, "nmod:to") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(1, 5, "nmod") should be(false)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(5, 4, "case") should be(true)
  }

  // TODO
}
