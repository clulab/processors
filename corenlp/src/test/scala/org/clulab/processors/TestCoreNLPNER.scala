package org.clulab.processors

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.scalatest.{FlatSpec, Matchers}

class TestCoreNLPNER extends FlatSpec with Matchers {
  val proc = new FastNLPProcessor()

  "CoreNLP" should "recognize NEs correctly in 1 sentence" in {
    val doc = proc.mkDocument("John Doe went to China on January 15th, 2001.", keepText = true)
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.recognizeNamedEntities(doc)

    doc.sentences(0).entities.get(0) should be("PERSON")
    doc.sentences(0).entities.get(1) should be("PERSON")
    doc.sentences(0).entities.get(2) should be("O")
    doc.sentences(0).entities.get(3) should be("O")
    doc.sentences(0).entities.get(4) should be("COUNTRY")
    doc.sentences(0).entities.get(5) should be("O")
    doc.sentences(0).entities.get(6) should be("DATE")
    doc.sentences(0).entities.get(7) should be("DATE")
    doc.sentences(0).entities.get(8) should be("DATE")
    doc.sentences(0).entities.get(9) should be("DATE")
    doc.sentences(0).entities.get(10) should be("O")

    doc.sentences(0).norms.get(5) should be("O")
    doc.sentences(0).norms.get(6) should be("2001-01-15")
    doc.sentences(0).norms.get(7) should be("2001-01-15")
    doc.sentences(0).norms.get(8) should be("2001-01-15")
    doc.sentences(0).norms.get(9) should be("2001-01-15")
    doc.sentences(0).norms.get(10) should be("O")
  }

  it should "recognize NEs correctly in 2 sentences" in {
    val doc = proc.mkDocument("John Doe went to China on January 15th, 2001. There, he visited family on January 20, 2001.", keepText = true)
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.recognizeNamedEntities(doc)

    doc.sentences(0).entities.get(0) should be("PERSON")
    doc.sentences(0).entities.get(1) should be("PERSON")
    doc.sentences(0).entities.get(2) should be("O")
    doc.sentences(0).entities.get(3) should be("O")
    doc.sentences(0).entities.get(4) should be("COUNTRY")
    doc.sentences(0).entities.get(5) should be("O")
    doc.sentences(0).entities.get(6) should be("DATE")
    doc.sentences(0).entities.get(7) should be("DATE")
    doc.sentences(0).entities.get(8) should be("DATE")
    doc.sentences(0).entities.get(9) should be("DATE")
    doc.sentences(0).entities.get(10) should be("O")
    doc.sentences(1).entities.get(6) should be ("DATE")
    doc.sentences(1).entities.get(7) should be ("DATE")
    doc.sentences(1).entities.get(8) should be ("DATE")
    doc.sentences(1).entities.get(9) should be ("DATE")

    doc.sentences(0).norms.get(5) should be("O")
    doc.sentences(0).norms.get(6) should be("2001-01-15")
    doc.sentences(0).norms.get(7) should be("2001-01-15")
    doc.sentences(0).norms.get(8) should be("2001-01-15")
    doc.sentences(0).norms.get(9) should be("2001-01-15")
    doc.sentences(0).norms.get(10) should be("O")
    doc.sentences(1).norms.get(6) should be("2001-01-20")
    doc.sentences(1).norms.get(7) should be("2001-01-20")
    doc.sentences(1).norms.get(8) should be("2001-01-20")
    doc.sentences(1).norms.get(9) should be("2001-01-20")
  }
}
