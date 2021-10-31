package org.clulab.processors.docs

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.FileUtils
import org.clulab.utils.Test

class TestDoc5501 extends Test {
  Utils.initializeDyNet()

  val file = "./main/src/test/resources/55015a60e1382326932d7199.txt"
  val text = FileUtils.getTextFromFile(file)
  val processor = new CluProcessor()

  behavior of "CluProcessor"

  it should "not crash while annotating" in {
    processor.annotate(text)
  }
}
