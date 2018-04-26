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
