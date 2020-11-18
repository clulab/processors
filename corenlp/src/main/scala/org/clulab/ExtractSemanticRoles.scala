package org.clulab

import java.io._
import java.util.Scanner

import org.clulab.processors.fastnlp.FastNLPProcessorWithSemanticRoles

import scala.io.Source

object ExtractSemanticRoles extends App {

  def findFiles(collectionDir: String, extension: String): Seq[File] = {
    val dir = new File(collectionDir)
    val filter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.endsWith(extension)
    }

    dir.listFiles(filter)
  }

  def getText(file: File): String = {
    val source = Source.fromFile(file, "utf-8")
    val text = source.mkString
    source.close()
    text
  }

  val inputDir = args(0)

  val files = findFiles(inputDir, "txt")
  val texts = files.map { file =>
    file.getName -> getText(file)
  }.toMap
  val fileNames = files.sortBy(_.length).map(_.getName).toArray

  val runtime = Runtime.getRuntime
  println(s"TotalMemory: ${runtime.totalMemory}")
  println(s"  MaxMemory: ${runtime.maxMemory}")
  println(s"  MAX_VALUE: ${Long.MaxValue}")
  val scanner = new Scanner(System.in)

  val processor = new FastNLPProcessorWithSemanticRoles {
    annotate("This is a test.")
  }

  def extractFrom(fileName: String): Unit = {
    // Include a self loop.
    println(s"Extracting from ${fileName}")
    println(s" FreeMemory: ${runtime.freeMemory}")
    val toText = texts(fileName)
    processor.annotate(toText, true)
  }

  fileNames.zipWithIndex.par.foreach { case (fromFileName, fromIndex) =>
    fileNames.zipWithIndex.par.foreach { case (toFileName, toIndex) =>
      if (toIndex == fromIndex)
        // Include a self loop.
        extractFrom(toFileName)
      else if (toIndex > fromIndex) {
        extractFrom(fromFileName)
        extractFrom(toFileName)
      }
    }
    // Close the last loop
    extractFrom(fromFileName)
  }
//  extractFrom(fileNames.last)
}
