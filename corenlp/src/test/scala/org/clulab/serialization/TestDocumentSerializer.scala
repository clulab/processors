package org.clulab.serialization

import org.clulab.processors.Processor
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.scalatest._

class TestDocumentSerializer extends FlatSpec with Matchers {
  val text = "John Doe went to China. There, he visited Beijing."
  val proc: Processor = new CoreNLPProcessor(withDiscourse = ShallowNLPProcessor.WITH_DISCOURSE)
  val ser = new DocumentSerializer

  "DocumentSerializer" should "save/load documents correctly, with defaults" in {
    val doc1 = proc.annotate(text)
    //println("Created a document with " + doc1.sentences.size + " sentences.")
    val out1 = ser.save(doc1)
    //println(out1)

    val doc2 = ser.load(out1)
    (doc2.sentences.size) should be (doc1.sentences.size)
    (doc2.text) should be (empty)

    val out2 = ser.save(doc2)
    (out2) should be (out1)
  }

  "DocumentSerializer" should "save/load documents correctly, keeping text" in {
    val doc11 = proc.annotate(text, true)    // explicit keep text
    //println("Created a document with " + doc11.sentences.size + " sentences.")
    val out11 = ser.save(doc11, keepText=true)
    // println(out11)                          // DEBUGGING

    val doc22 = ser.load(out11)
    (doc22.sentences.size) should be (doc11.sentences.size)
    (doc22.text) should not be (empty)
    (doc22.text.get) should be (text)

    val out22 = ser.save(doc22, keepText=true)
    // println(out22)                          // DEBUGGING
    (out22) should be (out11)
  }

}
