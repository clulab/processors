package org.clulab.processors.docs

import org.clulab.processors.clu.BalaurProcessor
import org.clulab.utils.FileUtils
import org.clulab.utils.Test

class TestDoc5da3 extends Test {
  val file = "./main/src/test/resources/5da30479998e17af8253786f.txt"
  val text = FileUtils.getTextFromFile(file)
  val processor = new BalaurProcessor()

  behavior of "BalaurProcessor"

  it should "not crash while annotating" in {
    processor.annotate(text)
  }
}
