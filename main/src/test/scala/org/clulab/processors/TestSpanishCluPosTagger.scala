package org.clulab.processors

import org.clulab.processors.clu.SpanishCluProcessor
import org.clulab.sequences.{ColumnsToDocument, SequenceTaggerEvaluator}
import org.scalatest.{FlatSpec, Matchers}

class TestSpanishCluPosTagger extends FlatSpec with Matchers {
  // TODO: see TestCluPosTagger
  val proc = new SpanishCluProcessor()

  "SpanishCluProcessor" should "POS tag UD sentences with an accuracy over 90%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/es_test.conllu")
    val doc = ColumnsToDocument.readFromStream(stream,
      setLabels = ColumnsToDocument.setTags,
      annotate = ColumnsToDocument.annotateLemmas)
    val acc = (new SequenceTaggerEvaluator[String, String]).accuracy(proc.posTagger, List(doc).iterator)
    println(s"POS tagger accuracy is $acc")
    (acc > 90) should be (true)
  }
}
