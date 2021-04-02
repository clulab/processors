package org.clulab.processors

import org.clulab.processors.examples.ParallelProcessorExample

import java.io.File
import scala.collection.mutable

object TestParallelApp extends App {

  def save(map: mutable.HashMap[String, String], file: File, value: String): Unit = {
    println(s" Completed ${file.getName}.")
    map(file.getName) = value
  }

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
  val serResults = mutable.HashMap.empty[String, String]

  println("Starting processing in parallel...")

  while (true)
    ParallelProcessorExample.mainWithCallback(Array(inputDir, outputDir, extension, "2")) { case (file, contents) =>
      save(parResults, file, contents)
    }
  println("Parallel processing complete.")

  println("Starting processing serially...")
  ParallelProcessorExample.mainWithCallback(Array(inputDir, outputDir, extension, "1")) { case (file, contents) =>
    save(serResults, file, contents)
  }
  println("Serial processing complete.")
}
