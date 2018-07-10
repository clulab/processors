package org.clulab.processors

import org.clulab.processors.clu.BioCluProcessor
import org.clulab.sequences.{ColumnsToDocument, SequenceTaggerEvaluator}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Integration test for the POS tagger
  * User: mihais
  * Date: 7/6/17
  */
class TestCluPosTagger extends FlatSpec with Matchers {
  val proc = new BioCluProcessor()

  "CluProcessor" should "POS tag WSJ with an accuracy over 97.0%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/wsj_test.conllx")
    val doc = ColumnsToDocument.readFromStream(stream,
      setLabels = ColumnsToDocument.setTags,
      annotate = ColumnsToDocument.annotateLemmas)
    val acc = (new SequenceTaggerEvaluator[String, String]).accuracy(proc.posTagger, List(doc).iterator, saveOutput = false)
    println(s"POS tagger accuracy is $acc")
    (acc > 97.0) should be (true)
  }

  it should "POS tag Genia with an accuracy over 98%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/genia_test.conllx")
    val doc = ColumnsToDocument.readFromStream(stream,
      setLabels = ColumnsToDocument.setTags,
      annotate = ColumnsToDocument.annotateLemmas)
    val acc = (new SequenceTaggerEvaluator[String, String]).accuracy(proc.posTagger, List(doc).iterator, saveOutput = false)
    println(s"POS tagger accuracy is $acc")
    (acc > 98.0) should be (true)
  }
}
