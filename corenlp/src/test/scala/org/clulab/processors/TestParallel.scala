package org.clulab.processors

import java.io.File
import org.clulab.processors.examples.ParallelProcessorExample
import org.clulab.utils.FileUtils
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.collection.mutable

class TestParallel extends FlatSpec with Matchers {

  def save(map: mutable.HashMap[String, String], file: File, value: String): Unit = {
    println(s" Completed ${file.getName}.")
    map(file.getName) = value
  }

  behavior of "Processing documents in parallel"

  it should "match processing documents serially" in {
    val inputDir = FileUtils.getSubprojectDir("./corenlp/src/test/resources/documents")
    val outputDir = "."
    val extension = "txt"

    val parResults = mutable.HashMap.empty[String, String]
    val serResults = mutable.HashMap.empty[String, String]

    println("Starting processing in parallel...")
    ParallelProcessorExample.mainWithCallback(Array(inputDir, outputDir, extension, "2")) { case (file, contents) =>
      save(parResults, file, contents)
    }
    println("Parallel processing complete.")

    println("Starting processing serially...")
    ParallelProcessorExample.mainWithCallback(Array(inputDir, outputDir, extension, "1")) { case (file, contents) =>
      save(serResults, file, contents)
    }
    println("Serial processing complete.")

    parResults should contain theSameElementsAs serResults
  }
}
