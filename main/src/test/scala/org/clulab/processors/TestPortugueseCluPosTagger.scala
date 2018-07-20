package org.clulab.processors

import org.clulab.processors.clu.PortugueseCluProcessor
import org.clulab.sequences.{ColumnsToDocument, SequenceTaggerEvaluator}
import org.scalatest.{FlatSpec, Matchers}

class TestPortugueseCluPosTagger extends FlatSpec with Matchers {
  // TODO: see TestCluPosTagger
  val proc = new PortugueseCluProcessor()

  "PortugueseCluProcessor" should "POS tag UD sentences with an accuracy over 96.6%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/pt_test.conllu")
    val doc = ColumnsToDocument.readFromStream(stream,
      wordPos = ColumnsToDocument.WORD_POS_CONLLU,
      labelPos = ColumnsToDocument.TAG_POS_CONLLU,
      setLabels = ColumnsToDocument.setTags,
      annotate = ColumnsToDocument.annotateLemmas,
      filterOutContractions = true)
    val acc = new SequenceTaggerEvaluator[String, String].accuracy(proc.posTagger, List(doc).iterator)
    println(s"POS tagger accuracy is $acc")
    (acc > 96.6) should be (true)
  }

}
