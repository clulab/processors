package edu.arizona.sista.processors

import java.io.{FileReader, BufferedReader}

import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._

/**
 * Tests BioNLPProcessor on the processing of an actual paper
 * User: mihais
 * Date: 11/16/14
 */
class TestBioNLPProcessor2 extends AssertionsForJUnit {
  var proc:Processor = new BioNLPProcessor()

  def testAbstract(): Unit = {
    val text = textFileToString("src/test/resources/edu/arizona/sista/processors/PLoS_One_2013_Dec_18_8_12_e84604.abstract.txt")
    val doc = proc.annotate(text)
    println(s"Generated a doc with ${doc.sentences.size} sentences.")
    assertTrue(doc.sentences.size == 7)
    assertTrue(doc.sentences(0).syntacticTree.isDefined)
  }

  @Test def testBody(): Unit = {
    val text = textFileToString("src/test/resources/edu/arizona/sista/processors/PLoS_One_2013_Dec_18_8_12_e84604.body.txt")
    val doc = annotate(text)
  }

  def annotate(text:String):Document = {
    val doc = proc.mkDocument(text)
    println(s"Processing a document with ${doc.sentences.size} sentences...")
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.recognizeNamedEntities(doc)
    proc.parse(doc)
    doc.clear()
    doc
  }

  def textFileToString(fn:String):String = {
    val b = new BufferedReader(new FileReader(fn))
    val out = new StringBuilder
    var done = false
    while(! done) {
      val l = b.readLine()
      if(l == null) {
        done = true
      } else {
        out.append(l)
        out.append("\n")
      }
    }
    b.close()
    out.toString()
  }
}
