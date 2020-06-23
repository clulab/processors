package org.clulab.processors

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.scalatest._

import scala.io.Source

class TestFastNLPProcessorEnv extends FlatSpec with Matchers {

  def mkDocument(text: String, date: String): Document = {
    val tokenizedDoc = proc.mkDocument(text, keepText = true)

    tokenizedDoc.setDCT(date)
    tokenizedDoc
  }

  def annotate(tokenizedDoc: Document): String = {
    val annotatedDoc: Document = proc.annotate(tokenizedDoc)
    val norms = annotatedDoc.sentences.flatMap(_.norms.get).mkString("\n")

    norms
  }

  var proc: Processor = new FastNLPProcessor()

  "FastNLPProcessor" should "get the same answer when reannotating a document" in {
      val inputDir = "./corenlp/src/test/resources/documents"

      val text1 = Source.fromFile(inputDir + "/1742.txt", "UTF-8").mkString // unclosed
      val text2 = Source.fromFile(inputDir + "/73f3.txt", "UTF-8").mkString // unclosed

      val date1 = "2012-09-22"
      val date2 = "2010-06-08"

      val tokenizedDoc1a = mkDocument(text1, date1)
      val tokenizedDoc1b = mkDocument(text1, date1)
      val tokenizedDoc2  = mkDocument(text2, date2)

      val expected = annotate(tokenizedDoc1a)
      annotate(tokenizedDoc2)
      val actual = annotate(tokenizedDoc1b)

      expected should be (actual)
  }
}
