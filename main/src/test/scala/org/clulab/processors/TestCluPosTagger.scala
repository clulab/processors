package org.clulab.processors

import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.clu.sequences.{ColumnsToDocument, SequenceTaggerEvaluator}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Integration test for the POS tagger
  * User: mihais
  * Date: 7/6/17
  */
class TestCluPosTagger extends FlatSpec with Matchers {
  val proc = new CluProcessor

  "CluProcessor" should "POS tag WSJ with an accuracy over 96%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/wsj_test.conllx")
    val doc = ColumnsToDocument.readFromStream(stream)
    val acc = (new SequenceTaggerEvaluator[String, String]).accuracy(proc.posTagger, List(doc).iterator)
    println(s"POS tagger accuracy is $acc")
    (acc > 96.0) should be (true)
  }

  it should "POS tag Genia with an accuracy over 95%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/genia_test.conllx")
    val doc = ColumnsToDocument.readFromStream(stream)
    val acc = (new SequenceTaggerEvaluator[String, String]).accuracy(proc.posTagger, List(doc).iterator)
    println(s"POS tagger accuracy is $acc")
    (acc > 85.0) should be (true) // TODO: set to 95 when new model is available
  }
}
