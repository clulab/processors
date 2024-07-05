package org.clulab.processors.docs

import org.clulab.processors.clu.BalaurProcessor
import org.clulab.utils.FileUtils
import org.clulab.utils.Test

class TestDoc58b0 extends Test {
  val file = "./library/src/test/resources/58b0a61ccf58f11d2292edec.txt"
  val text = FileUtils.getTextFromFile(file)
  val processor = new BalaurProcessor()

  behavior of "BalaurProcessor"

  it should "not crash while annotating" in {
    processor.annotate(text)
  }
}
