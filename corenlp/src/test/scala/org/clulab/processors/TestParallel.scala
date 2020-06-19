package org.clulab.processors

import org.clulab.processors.examples.ParallelProcessorExample
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TestParallel extends FlatSpec with Matchers {

  "Processing documents in parallel" should "not cause crash" in {
    ParallelProcessorExample.main(Array("./corenlp/src/test/resources/documents", ".", "txt", "2"))
  }
}
