package org.clulab.processors

import org.clulab.processors.clu.SpanishCluProcessor
import org.scalatest.{FlatSpec, Matchers}

class TestSpanishCluProcessor extends FlatSpec with Matchers {
  val proc = new SpanishCluProcessor()

  "SpanishCluProcessor" should "tokenize raw text correctly" in {
    val doc = proc.mkDocument("Juan Olivarez viajó a China. Él visitó Pekín allí.")
    doc.clear()

    doc.sentences(0).words(0) should be ("Juan")
    doc.sentences(0).words(1) should be ("Olivarez")
    doc.sentences(0).words(2) should be ("viajó")
    doc.sentences(0).words(3) should be ("a")
    doc.sentences(0).words(4) should be ("China")
    doc.sentences(0).words(5) should be (".")
    doc.sentences(1).words(0) should be ("Él")
    doc.sentences(1).words(1) should be ("visitó")
    doc.sentences(1).words(2) should be ("Pekín")
    doc.sentences(1).words(3) should be ("allí")
    doc.sentences(1).words(4) should be (".")

    doc.sentences(0).startOffsets(0) should be (0)
    doc.sentences(0).startOffsets(1) should be (5)
    doc.sentences(0).startOffsets(2) should be (14)
    doc.sentences(0).startOffsets(3) should be (20)
    doc.sentences(0).startOffsets(4) should be (22)
    doc.sentences(0).startOffsets(5) should be (27)
    doc.sentences(1).startOffsets(0) should be (29)
    doc.sentences(1).startOffsets(1) should be (32)
    doc.sentences(1).startOffsets(2) should be (39)
    doc.sentences(1).startOffsets(3) should be (45)
    doc.sentences(1).startOffsets(4) should be (49)
  }

  it should "POS tag correctly" in {
    val doc = proc.annotate("Juan Olivarez viajó a China. Él visitó Pekín allí.")

    doc.sentences(0).tags.get(0) should be ("PROPN")
    doc.sentences(0).tags.get(1) should be ("PROPN")
    doc.sentences(0).tags.get(2) should be ("VERB")
    doc.sentences(0).tags.get(3) should be ("ADP")
    doc.sentences(0).tags.get(4) should be ("PROPN")
    doc.sentences(0).tags.get(5) should be ("PUNCT")
    doc.sentences(1).tags.get(0) should be ("PRON")
    doc.sentences(1).tags.get(1) should be ("VERB")
    doc.sentences(1).tags.get(2) should be ("PROPN")
    doc.sentences(1).tags.get(3) should be ("ADV")
    doc.sentences(1).tags.get(4) should be ("PUNCT")
  }

  /*
  it should "lemmatize correctly" in {
    // TODO: Spanish lemmatization
  }

  it should "recognize syntactic chunks correctly" in {
    // TODO: Spanish chunking
  }
  */

  it should "parse text correctly" in {
    val doc = proc.annotate("Juan Olivarez viajó a China. Él visitó Pekín allí.")

    doc.sentences(0).universalBasicDependencies.get.hasEdge(2, 0, "nsubj") should be(true)
    doc.sentences(0).universalBasicDependencies.get.hasEdge(0, 1, "flat") should be(true)
    doc.sentences(0).universalBasicDependencies.get.hasEdge(2, 4, "obl") should be(true)
    doc.sentences(0).universalBasicDependencies.get.hasEdge(4, 3, "case") should be(true)
    doc.sentences(0).universalBasicDependencies.get.hasEdge(2, 5, "punct") should be(true)

    doc.sentences(1).universalBasicDependencies.get.hasEdge(1, 0, "nsubj") should be(true)
    doc.sentences(1).universalBasicDependencies.get.hasEdge(1, 2, "obj") should be(true)
    doc.sentences(1).universalBasicDependencies.get.hasEdge(1, 3, "advmod") should be(true)
    doc.sentences(1).universalBasicDependencies.get.hasEdge(1, 4, "punct") should be(true)
  }
}
