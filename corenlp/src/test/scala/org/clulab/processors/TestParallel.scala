package org.clulab.processors

import org.clulab.processors.examples.ParallelProcessorExample
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.slf4j.{Logger, LoggerFactory}

import TestParallel._

class TestParallel extends FlatSpec with Matchers {

  "Processing documents in parallel" should "not cause crash" in {
    logger.info("Starting processing...")
    ParallelProcessorExample.main(Array("./corenlp/src/test/resources/documents", ".", "txt", "2"))
    logger.info("Processing complete.")
  }
}

object TestParallel {
  val logger:Logger = LoggerFactory.getLogger(classOf[TestParallel])
}
