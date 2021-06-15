package org.clulab.numeric

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.struct.Interval
import org.scalatest.{FlatSpec, Matchers}

class TestDateRecognition extends FlatSpec with Matchers {
  Utils.initializeDyNet()
  val ner = new NumericEntityRecognizer
  val proc = new CluProcessor()

  //
  // unit tests here
  //

  "the numeric entity recognizer" should "recognize dates in the European format" in {
    val (words, ents, norms) = numericParse("It is 12 May, 2000")
    ensure(words, ents, norms, Interval(2, 6), "DATE", "2000-05-12")
  }

  //
  // Help methods below this point
  //

  /** Makes sure that the given span has the right entity labels and norms */
  def ensure(words: Array[String],
             entities: Array[String],
             norms: Array[String],
             span: Interval,
             goldEntity: String,
             goldNorm: String): Unit = {
    println("Verifying the following text:")
    println("Words:    " + words.mkString(", "))
    println("Entities: " + entities.mkString(", "))
    println("Norms:    " + norms.mkString(", "))

    var first = true
    for(i <- span.indices) {
      val prefix = if(first) "B-" else "I-"
      val label = prefix + goldEntity

      entities(i) should be (label)
      norms(i) should be (goldNorm)

      first = false
    }
  }

  /** Runs the actual numeric entity recognizer */
  def numericParse(sentence: String): (Array[String], Array[String], Array[String]) = {
    val doc = proc.annotate(sentence)
    val mentions = ner.extractFrom(doc)
    setLabelsAndNorms(doc, mentions)

    // assume 1 sentence per doc
    val sent = doc.sentences.head
    Tuple3(sent.words, sent.entities.get, sent.norms.get)
  }
}
