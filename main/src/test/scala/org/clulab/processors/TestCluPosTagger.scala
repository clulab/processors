package org.clulab.processors

import org.clulab.processors.clulab.CluProcessor
import org.clulab.processors.clulab.sequences.{ColumnsToDocument, SequenceTaggerEvaluator}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Integration test for the POS tagger
  * User: mihais
  * Date: 7/6/17
  */
class TestCluPosTagger extends FlatSpec with Matchers {
  val proc = new CluProcessor

  "CluProcessor" should "POS tag with an accuracy over 96%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/sec23.tagged")
    val doc = ColumnsToDocument.readFromStream(stream, 0, 1)
    val acc = (new SequenceTaggerEvaluator[String, String]).accuracy(proc.posTagger, List(doc).iterator)
    println(s"POS tagger accuracy is $acc")
    (acc > 96.0) should be (true)
  }
}
