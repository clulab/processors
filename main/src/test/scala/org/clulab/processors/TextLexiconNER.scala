package org.clulab.processors

import org.clulab.processors.clu.CluProcessor
import org.clulab.sequences.LexiconNER
import org.scalatest.{FlatSpec, Matchers}

/**
  * Tests the LexiconNER
  * User: mihais
  * Date: 10/3/17
  */
class TextLexiconNER extends FlatSpec with Matchers {
  val proc = new CluProcessor()

  "LexiconNER" should "match categories correctly" in {
    val ner = LexiconNER(List("org/clulab/processors/A.tsv", "org/clulab/processors/B.tsv"))

    var doc = annotate("a a b b a")
    var labels = ner.find(doc.sentences.head)
    println(s"Labels: ${labels.mkString(", ")}")
    labels.length should be (5)
    labels(0) should be ("B-A")
    labels(1) should be ("I-A")
    labels(2) should be ("B-B")
    labels(3) should be ("I-B")
    labels(4) should be ("O")

    doc = annotate("a a a b b a")
    labels = ner.find(doc.sentences.head)
    println(s"Labels: ${labels.mkString(", ")}")
    labels.length should be (6)
    labels(0) should be ("B-B")
    labels(1) should be ("I-B")
    labels(2) should be ("I-B")
    labels(3) should be ("B-B")
    labels(4) should be ("I-B")
    labels(5) should be ("O")
  }

  def annotate(text:String):Document = {
    val doc = proc.mkDocument(text)
    proc.lemmatize(doc)
    doc
  }
}
