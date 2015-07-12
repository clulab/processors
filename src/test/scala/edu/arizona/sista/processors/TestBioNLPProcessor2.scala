package edu.arizona.sista.processors

import java.io.{FileReader, BufferedReader}

import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import org.scalatest._

/**
 * Tests BioNLPProcessor on the processing of an actual paper
 * User: mihais
 * Date: 11/16/14
 */
class TestBioNLPProcessor2 extends FlatSpec with Matchers {
  var proc:Processor = new BioNLPProcessor()

  "BioNLPProcessor" should "parse abstract text" in {
    val text = textFileToString("src/test/resources/edu/arizona/sista/processors/PLoS_One_2013_Dec_18_8_12_e84604.abstract.txt")
    val doc = proc.annotate(text)
    //println(s"Generated a doc with ${doc.sentences.size} sentences.")
    doc.sentences.size should be (7)
    doc.sentences(0).syntacticTree.isDefined should be (true)
  }

  it should "parse body text" in {
    val text = textFileToString("src/test/resources/edu/arizona/sista/processors/PLoS_One_2013_Dec_18_8_12_e84604.body.txt")
    val doc = annotate(text)
    doc.sentences(0).syntacticTree.isDefined should be (true)
  }

  def annotate(text:String):Document = {
    val doc = proc.mkDocument(text, keepText = false)
    //println(s"Processing a document with ${doc.sentences.size} sentences...")
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
