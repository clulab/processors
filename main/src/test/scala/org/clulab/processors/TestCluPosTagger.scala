package org.clulab.processors

import org.clulab.processors.clulab.CluProcessor
import org.scalatest.{FlatSpec, Matchers}

/**
  * Integration test for the POS tagger
  * User: mihais
  * Date: 7/6/17
  */
class TestCluPosTagger extends FlatSpec with Matchers {
  val proc = new CluProcessor

  "CluProcessor" should "POS tag with an accuracy over 96%" in {
    
  }
}
