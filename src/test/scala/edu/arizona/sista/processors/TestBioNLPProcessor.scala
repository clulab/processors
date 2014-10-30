package edu.arizona.sista.processors

import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

/**
 *
 * User: mihais
 * Date: 10/29/14
 */
class TestBioNLPProcessor extends AssertionsForJUnit {
  var proc:Processor = new BioNLPProcessor()

  @Test def testNER() {
    val doc = proc.mkDocumentFromSentences(List("Co-immunoprecipitation analysis confirmed that Bis interacted with Bcl-2 in vivo."))
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.recognizeNamedEntities(doc)
    doc.clear()

    for(ne <- doc.sentences(0).entities.get) {
      println(s"NE: $ne")
    }
  }

}
