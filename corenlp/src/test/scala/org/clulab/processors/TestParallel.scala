package org.clulab.processors

import java.io.File

import org.clulab.processors.examples.ParallelProcessorExample
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

class TestParallel extends FlatSpec with Matchers {
  import TestParallel._

  def save(map: mutable.HashMap[String, String], file: File, value: String): Unit =
    map(file.getName) = value

  behavior of "Processing documents in parallel"

  it should "match processing documents serially" in {
    val inputDir = "./corenlp/src/test/resources/documents"
    val outputDir = "."
    val extension = "txt"

    val parResults = mutable.HashMap.empty[String, String]
    val serResults = mutable.HashMap.empty[String, String]

    logger.info("Starting processing in parallel...")
    ParallelProcessorExample.mainWithCallback(Array(inputDir, outputDir, extension, "2")) { case (file, contents) =>
      save(parResults, file, contents)
    }
    logger.info("Parallel processing complete.")

    logger.info("Starting processing serially...")
    ParallelProcessorExample.mainWithCallback(Array(inputDir, outputDir, extension, "1")) { case (file, contents) =>
      save(serResults, file, contents)
    }
    logger.info("Serial processing complete.")

    parResults should contain theSameElementsAs serResults
  }
}

object TestParallel {
  val logger:Logger = LoggerFactory.getLogger(classOf[TestParallel])
}
