package org.clulab.processors

import java.io.File
import org.clulab.processors.examples.ParallelProcessorExample
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.FileUtils
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.collection.mutable

class TestParallel extends FlatSpec with Matchers {

  def save(map: mutable.HashMap[String, String], file: File, value: String, par: Boolean): Unit = {
    println(s" Completed ${file.getName}.")
    map(file.getName) = value

    val outFile = new File((if (par) "./par/" else "./ser/") + file.getName)
    FileUtils.printWriterFromFile(outFile).autoClose { printWriter =>
      printWriter.println(value)
    }
  }

  behavior of "Processing documents in parallel"

  it should "match processing documents serially" in {
    val inputDir = FileUtils.getSubprojectDir("./corenlp/src/test/resources/documents1")
    val outputDir = "."
    val extension = "txt"

    val parResults = {
      val parResults = mutable.HashMap.empty[String, String]
      println("Starting processing in parallel...")
      ParallelProcessorExample.mainWithCallback(Array(inputDir, outputDir, extension, "1", "true")) { case (file, contents) =>
        save(parResults, file, contents, true)
      }
      println("Parallel processing complete.")
      parResults
    }

    val serResults = {
      val serResults = mutable.HashMap.empty[String, String]
      println("Starting processing serially...")
      ParallelProcessorExample.mainWithCallback(Array(inputDir, outputDir, extension, "1", "false")) { case (file, contents) =>
        save(serResults, file, contents, false)
      }
      println("Serial processing complete.")
      serResults
    }

    parResults should contain theSameElementsAs serResults
  }
}
