package org.clulab.processors

import org.clulab.processors.clu.PortugueseCluProcessor
import org.clulab.sequences.{ColumnsToDocument, SequenceTaggerEvaluator}
import org.scalatest.{FlatSpec, Matchers}

class TestPortugueseCluPosTagger extends FlatSpec with Matchers {
  val proc = new PortugueseCluProcessor()

  "PortugueseCluProcessor" should "POS tag Bosque sentences with an accuracy over 96.4%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/pt_bosque_ud_test.conllu")
    val doc = ColumnsToDocument.readFromStream(stream,
      wordPos = ColumnsToDocument.WORD_POS_CONLLU,
      labelPos = ColumnsToDocument.TAG_POS_CONLLU,
      setLabels = ColumnsToDocument.setTags,
      annotate = ColumnsToDocument.annotateLemmas,
      filterOutContractions = true)
    val acc = new SequenceTaggerEvaluator[String, String].accuracy(proc.posTagger, List(doc).iterator, saveOutput = false)
    println(s"POS tagger accuracy is $acc")
    acc should be > 96.4
  }

  "PortugueseCluProcessor" should "POS tag GSD sentences with an accuracy over 94.7%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/pt_gsd_ud_test.conllu")
    val doc = ColumnsToDocument.readFromStream(stream,
      wordPos = ColumnsToDocument.WORD_POS_CONLLU,
      labelPos = ColumnsToDocument.TAG_POS_CONLLU,
      setLabels = ColumnsToDocument.setTags,
      annotate = ColumnsToDocument.annotateLemmas,
      filterOutContractions = true)
    val acc = new SequenceTaggerEvaluator[String, String].accuracy(proc.posTagger, List(doc).iterator, saveOutput = false)
    println(s"POS tagger accuracy is $acc")
    acc should be > 94.7
  }

  "PortugueseCluProcessor" should "POS tag PUD sentences with an accuracy over 88.5%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/pt_pud_ud_test.conllu")
    val doc = ColumnsToDocument.readFromStream(stream,
      wordPos = ColumnsToDocument.WORD_POS_CONLLU,
      labelPos = ColumnsToDocument.TAG_POS_CONLLU,
      setLabels = ColumnsToDocument.setTags,
      annotate = ColumnsToDocument.annotateLemmas,
      filterOutContractions = true)
    val acc = new SequenceTaggerEvaluator[String, String].accuracy(proc.posTagger, List(doc).iterator, saveOutput = false)
    println(s"POS tagger accuracy is $acc")
    acc should be > 88.5
  }

}
