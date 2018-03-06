package org.clulab.processors

import org.clulab.processors.clu.CluProcessor
import org.clulab.sequences.{ColumnsToDocument, SequenceTaggerEvaluator}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Integration test for the syntactic chunker
  */
class TestCluChunker extends FlatSpec with Matchers {
  val proc = new CluProcessor()

  "CluProcessor" should "chunk text with an accuracy over 97%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/chunking_test.conllx")
    val doc = ColumnsToDocument.readFromStream(stream,
      wordPos = 0, labelPos = 2,
      setLabels = ColumnsToDocument.setChunks,
      annotate = ColumnsToDocument.annotateLemmmaTags)
    val acc = (new SequenceTaggerEvaluator[String, String]).accuracy(proc.chunker, List(doc).iterator, saveOutput = false)
    println(s"Chunker accuracy is $acc")
    (acc > 95.6) should be (true)
  }

}
