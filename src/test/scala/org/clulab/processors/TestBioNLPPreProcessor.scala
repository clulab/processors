package org.clulab.processors

import org.clulab.processors.bionlp.BioNLPPreProcessor
import org.scalatest.{Matchers, FlatSpec}

/**
 * Tests the preprocessor
 * User: mihais
 * Date: 8/14/15
 */
class TestBioNLPPreProcessor extends FlatSpec with Matchers {
  val pp = new BioNLPPreProcessor(true)

  "BioNLPPreProcessor" should "convert Unicode to ASCII" in {
    val s = "\u2714alpha\u03B1\u25B6"
    val ps = pp.preprocess(s)

    ps should be ("valphaalpha>")
  }
}
