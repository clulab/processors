package org.clulab.processors

import org.clulab.processors.clu.SpanishCluProcessor
import org.scalatest.{FlatSpec, Matchers}

class TestSpanishCluProcessor extends FlatSpec with Matchers {
  val proc = new SpanishCluProcessor()

  "SpanishCluProcessor" should "tokenize raw text correctly" in {
    val doc = proc.mkDocument("Juan Olivarez se fue a China. Alli, el visito Beijing.")
    doc.clear()

    doc.sentences(0).words(0) should be ("Juan")
    doc.sentences(0).words(1) should be ("Olivarez")
    doc.sentences(0).words(2) should be ("se")
    doc.sentences(0).words(3) should be ("fue")
    doc.sentences(0).words(4) should be ("a")
    doc.sentences(0).words(5) should be ("China")
    doc.sentences(0).words(6) should be (".")
    doc.sentences(1).words(0) should be ("Alli")
    doc.sentences(1).words(1) should be (",")
    doc.sentences(1).words(2) should be ("el")
    doc.sentences(1).words(3) should be ("visito")
    doc.sentences(1).words(4) should be ("Beijing")
    doc.sentences(1).words(5) should be (".")

    doc.sentences(0).startOffsets(0) should be (0)
    doc.sentences(0).startOffsets(1) should be (5)
    doc.sentences(0).startOffsets(2) should be (14)
    doc.sentences(0).startOffsets(3) should be (17)
    doc.sentences(0).startOffsets(4) should be (21)
    doc.sentences(0).startOffsets(5) should be (23)
    doc.sentences(0).startOffsets(6) should be (28)
    doc.sentences(1).startOffsets(0) should be (30)
    doc.sentences(1).startOffsets(1) should be (34)
    doc.sentences(1).startOffsets(2) should be (36)
    doc.sentences(1).startOffsets(3) should be (39)
    doc.sentences(1).startOffsets(4) should be (46)
    doc.sentences(1).startOffsets(5) should be (53)
    println("Tokenization is fine.")
  }

  it should "POS tag correctly" in {
    // TODO
  }

  it should "recognize syntactic chunks correctly" in {
    // TODO later
  }

  it should "parse text correctly" in {
    val doc = proc.annotate("John Doe se fue a China")

    println("Basic universal dependencies:")
    println(doc.sentences.head.universalBasicDependencies.get)

    // TODO
  }

}
