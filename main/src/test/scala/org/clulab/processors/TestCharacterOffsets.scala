package org.clulab.processors

import org.clulab.TestUtils

class TestCharacterOffsets extends FatdynetTest {

  "the tokenizer" should "generate correct character offsets in a single sentence" in {
    val doc = proc.mkDocument("John Doe went to China on January 15th, 2001.", keepText = true)

    val s = doc.sentences.head
    for(i <- s.indices) {
      val raw = s.raw(i)
      val rawFromOffsets = doc.text.get.substring(s.startOffsets(i), s.endOffsets(i))
      //println(s"Comparing $raw with $rawFromOffsets.")
      raw.equals(rawFromOffsets) should be (true)
    }
  }

  it should "generate correct character offsets in multiple sentences" in {
    val doc = proc.mkDocumentFromSentences(List("John Doe went to China.", "There, he visited Beijing."), keepText = true)

    for(s <- doc.sentences) {
      for(i <- s.indices) {
        val raw = s.raw(i)
        val rawFromOffsets = doc.text.get.substring(s.startOffsets(i), s.endOffsets(i))
        //println(s"Comparing $raw with $rawFromOffsets.")
        raw.equals(rawFromOffsets) should be (true)
      }
    }
  }

  it should "generate correct character offsets in a file" in {
    val text = TestUtils.readFile("org/clulab/processors/raw_text.txt")
    val doc = proc.mkDocument(text, keepText = true)

    for(s <- doc.sentences) {
      for(i <- s.indices) {
        val raw = s.raw(i)
        val rawFromOffsets = doc.text.get.substring(s.startOffsets(i), s.endOffsets(i))
        //println(s"Comparing $raw with $rawFromOffsets.")
        raw should equal(rawFromOffsets)
      }
    }
  }
}
