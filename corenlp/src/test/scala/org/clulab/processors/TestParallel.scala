package org.clulab.processors

import java.io.File
import org.clulab.processors.examples.ParallelProcessorExample
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
    // If fork is true in sbt, then . is already in corenlp directory, the subprojectDir.
    // This is not the case if fork is false, nor is it in IntelliJ with default settings.
    val relativeToProjectDir = "./corenlp/src/test/resources/documents"
    val relativeToSubprojectDir = "./src/test/resources/documents"
    val inputDir =
      if (new File(relativeToProjectDir).exists) relativeToProjectDir
      else relativeToSubprojectDir
    val outputDir = "."
    val extension = "txt"

    val parResults = mutable.HashMap.empty[String, String]
    val par1Results = mutable.HashMap.empty[String, String]
    val serResults = mutable.HashMap.empty[String, String]
    val ser1Results = mutable.HashMap.empty[String, String]

//    println("Starting processing in parallel...")
//    ParallelProcessorExample.mainWithCallback(Array(inputDir, outputDir, extension, "2")) { case (file, contents) =>
//      save(parResults, file, contents)
//    }
//
//    ParallelProcessorExample.mainWithCallback(Array(inputDir, outputDir, extension, "2")) { case (file, contents) =>
//      save(par1Results, file, contents)
//    }

    println("Parallel processing complete.")

    println("Starting processing serially...")
    ParallelProcessorExample.mainWithCallback(Array(inputDir, outputDir, extension, "1")) { case (file, contents) =>
      if (!serResults.contains(file.getName)) {
        println(s"End Pass 1 of ${file.getName} ============================================")
        save(serResults, file, contents)
      } else {
        println(s"End Pass 2 of ${file.getName} ============================================")
        save(ser1Results, file, contents)
      }
    }

//    ParallelProcessorExample.mainWithCallback(Array(inputDir, outputDir, extension, "1")) { case (file, contents) =>
//      save(ser1Results, file, contents)
//    }
//    println("Serial processing complete.")

//    parResults.keySet should contain theSameElementsAs serResults.keySet

//    parResults.keySet.foreach { key =>
//      val parValue = parResults(key)
//      val par1Value = par1Results(key)
//
//      if (parValue != par1Value)
//        println("They didn't match!")
//    }

    serResults.keySet.foreach { key =>
      val serValue = serResults(key)
      val ser1Value = ser1Results(key)

      if (serValue != ser1Value)
        println("They didn't match!")
    }

    parResults should contain theSameElementsAs serResults
  }
}
