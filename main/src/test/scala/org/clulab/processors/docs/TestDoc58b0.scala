package org.clulab.processors.docs

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.FileUtils
import org.clulab.utils.Test

class TestDoc58b0 extends Test {
  Utils.initializeDyNet()

  val file = "./main/src/test/resources/58b0a61ccf58f11d2292edec.txt"
  val text = FileUtils.getTextFromFile(file)
  val processor = new CluProcessor()

  behavior of "CluProcessor"

  it should "not crash while annotating" in {
    processor.annotate(text)
  }
}
