package org.clulab.processors

import edu.stanford.nlp.ling.CoreAnnotations.{SentencesAnnotation, TextAnnotation, TokensAnnotation}
import org.clulab.processors.corenlp.CoreNLPDocument
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.JavaConverters._

class TestCoreNLPNER extends FlatSpec with Matchers {
  val proc = new FastNLPProcessor()

  it should "recognize NEs correctly" in {
    val doc = proc.mkDocument("John Doe went to China on January 15th, 2001.", keepText = true)
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.recognizeNamedEntities(doc)

    doc.sentences(0).entities.get(0) should be("PERSON")
    doc.sentences(0).entities.get(1) should be("PERSON")
    doc.sentences(0).entities.get(2) should be("O")
    doc.sentences(0).entities.get(3) should be("O")
    doc.sentences(0).entities.get(4) should be("LOCATION")
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

    val docAnnotation = doc.asInstanceOf[CoreNLPDocument].annotation.get
    val sents = docAnnotation.get(classOf[SentencesAnnotation]).asScala
    val text = docAnnotation.get(classOf[TextAnnotation])
    println(s"Text: $text")
    for(s <- sents) {
      val tokens = s.get(classOf[TokensAnnotation]).asScala
      for(token <- tokens) {
        val word = token.word()
        val start = token.beginPosition()
        val end = token.endPosition()
        println(s"$word $start $end")
        val wordFromOffsets = text.substring(start, end)
        word should equal(wordFromOffsets)
      }
    }
  }
}
