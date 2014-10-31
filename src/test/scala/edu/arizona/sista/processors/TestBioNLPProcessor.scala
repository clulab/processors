package edu.arizona.sista.processors

import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._

/**
 *
 * User: mihais
 * Date: 10/29/14
 */
class TestBioNLPProcessor extends AssertionsForJUnit {
  var proc:Processor = new BioNLPProcessor()

  @Test def testNER() {
    val doc = proc.mkDocumentFromSentences(List(
      "Co-immunoprecipitation analysis confirmed that Bis interacted with Bcl-2 in vivo.",
      "The Ras protein is phosphorylated by TBRI."))
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.recognizeNamedEntities(doc)
    doc.clear()

    assertTrue(doc.sentences(0).entities.get(7) == "B-GENE")
    assertTrue(doc.sentences(1).entities.get(1) == "B-GENE")
    assertTrue(doc.sentences(1).entities.get(2) == "I-GENE")
    assertTrue(doc.sentences(1).entities.get(6) == "B-GENE")

    var i = 0
    for(s <- doc.sentences) {
      println(s"Sentence #$i")
      for (ne <- s.entities.get) {
        println(s"\tNE: $ne")
      }
      i += 1
    }
  }

}
