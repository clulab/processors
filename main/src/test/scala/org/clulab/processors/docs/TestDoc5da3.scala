package org.clulab.processors.docs

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.FileUtils
import org.clulab.utils.Test

class TestDoc5da3 extends Test {
  Utils.initializeDyNet()

  val file = "./main/src/test/resources/5da30479998e17af8253786f.txt"
  val text = FileUtils.getTextFromFile(file)
  val processor = new CluProcessor()

  behavior of "CluProcessor"

  it should "not crash while annotating" in {
    processor.annotate(text)
  }
}
