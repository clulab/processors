package edu.arizona.sista.processors

import corenlp.CoreNLPProcessor
import org.scalatest._

class TestDocumentSerializer extends FlatSpec with Matchers {
  "DocumentSerializer" should "save/load documents correctly" in {
    val text = "John Doe went to China. There, he visited Beijing."
    val proc:Processor = new CoreNLPProcessor(withDiscourse = true)
    val doc1 = proc.annotate(text)
    //println("Constructed a document with " + doc1.sentences.size + " sentences.")

    val ser = new DocumentSerializer
    val out1 = ser.save(doc1)
    //println("Generated annotations:")
    //println(out1)

    val doc2 = ser.load(out1)
    doc2.sentences.size should be (doc1.sentences.size)
    val out2 = ser.save(doc2)
    out1 should be (out2)
  }
}