package org.clulab.processors.docs

import org.clulab.processors.clu.BalaurProcessor
import org.clulab.utils.FileUtils
import org.clulab.utils.Test

class TestDoc5501 extends Test {
  val file = "./library/src/test/resources/55015a60e1382326932d7199.txt"
  val text = FileUtils.getTextFromFile(file)
  val processor = new BalaurProcessor()

  behavior of "BalaurProcessor"

  it should "not crash while annotating" in {
    processor.annotate(text)
  }
}
