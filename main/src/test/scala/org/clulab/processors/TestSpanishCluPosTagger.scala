package org.clulab.processors

import org.clulab.processors.clu.SpanishCluProcessor
import org.clulab.sequences.{ColumnsToDocument, SequenceTaggerEvaluator}
import org.scalatest.{FlatSpec, Matchers}

class TestSpanishCluPosTagger extends FlatSpec with Matchers {
  val proc = new SpanishCluProcessor()

  "SpanishCluProcessor" should "POS tag AnCora sentences with an accuracy over 96.4%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/es_ancora_ud_test.conllu")
    val doc = ColumnsToDocument.readFromStream(stream,
      wordPos = ColumnsToDocument.WORD_POS_CONLLU,
      labelPos = ColumnsToDocument.TAG_POS_CONLLU,
      setLabels = ColumnsToDocument.setTags,
      annotate = ColumnsToDocument.annotateLemmas,
      filterOutContractions = true)
    val acc = new SequenceTaggerEvaluator[String, String].accuracy(proc.posTagger, List(doc).iterator, saveOutput = false)
    println(s"POS tagger accuracy is $acc")
    (acc > 96.4) should be (true)
  }

  "SpanishCluProcessor" should "POS tag GSD sentences with an accuracy over 92.7%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/es_gsd_ud_test.conllu")
    val doc = ColumnsToDocument.readFromStream(stream,
      wordPos = ColumnsToDocument.WORD_POS_CONLLU,
      labelPos = ColumnsToDocument.TAG_POS_CONLLU,
      setLabels = ColumnsToDocument.setTags,
      annotate = ColumnsToDocument.annotateLemmas,
      filterOutContractions = true)
    val acc = new SequenceTaggerEvaluator[String, String].accuracy(proc.posTagger, List(doc).iterator, saveOutput = false)
    println(s"POS tagger accuracy is $acc")
    (acc > 92.7) should be (true)
  }

  "SpanishCluProcessor" should "POS tag PUD sentences with an accuracy over 88.8%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/es_pud_ud_test.conllu")
    val doc = ColumnsToDocument.readFromStream(stream,
      wordPos = ColumnsToDocument.WORD_POS_CONLLU,
      labelPos = ColumnsToDocument.TAG_POS_CONLLU,
      setLabels = ColumnsToDocument.setTags,
      annotate = ColumnsToDocument.annotateLemmas,
      filterOutContractions = true)
    val acc = new SequenceTaggerEvaluator[String, String].accuracy(proc.posTagger, List(doc).iterator, saveOutput = false)
    println(s"POS tagger accuracy is $acc")
    (acc > 88.8) should be (true)
  }
}
