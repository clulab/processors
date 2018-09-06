package org.clulab.processors

import org.clulab.processors.clu.PortugueseCluProcessor
import org.clulab.sequences.{ ColumnsToDocument, SequenceTaggerEvaluator }
import org.scalatest.{ FlatSpec, Matchers }

/**
  * Integration test for the syntactic chunker
  */
class TestPortugueseCluChunker extends FlatSpec with Matchers {

  val proc = new PortugueseCluProcessor()

  val minAccuracy = 93.2

  "PortugueseCluProcessor" should f"chunk text with an accuracy over ${minAccuracy}%.1f%%" in {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/pt_chunking_test.conllx")
    val doc = ColumnsToDocument.readFromStream(stream,
      wordPos = 0, labelPos = 2,
      setLabels = ColumnsToDocument.setChunks,
      annotate = ColumnsToDocument.annotateLemmmaTags)
    val acc = (new SequenceTaggerEvaluator[String, String]).accuracy(proc.chunker.get, List(doc).iterator, saveOutput = false)
    println(s"Chunker accuracy is $acc")
    acc should be > minAccuracy
  }

}
