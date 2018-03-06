package org.clulab.processors

import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.clu.sequences.NamedEntityRecognizer
import org.clulab.sequences.{ColumnsToDocument, SequenceTaggerEvaluator}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Integration test for the Clu NER
  */
class TestCluNER extends FlatSpec with Matchers {
  val proc = new CluProcessor()

  "CluProcessor" should "recognize entities with an accuracy over 97.7%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/eng.testa")
    val doc = ColumnsToDocument.readFromStream(stream,
      wordPos = 0, labelPos = 3,
      setLabels = ColumnsToDocument.setEntities,
      annotate = ColumnsToDocument.annotateLemmmaTags)
    val acc = (new SequenceTaggerEvaluator[String, String]).accuracy(proc.ner.get.asInstanceOf[NamedEntityRecognizer],
      List(doc).iterator, saveOutput = false)
    println(s"NER accuracy is $acc")
    (acc > 97.7) should be (true)
  }

}
