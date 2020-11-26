package org.clulab.processors

import java.io._
import java.util.Scanner

import org.clulab.processors.fastnlp.FastNLPProcessorWithSemanticRoles

import scala.io.Source

object ExtractSemanticRolesSingle extends App {

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
  val extension = args(1)
  val count = args(2).toInt
  val threads = args(3).toInt

  val files = {
    val files = findFiles(inputDir, extension)
    val file = files.drop(2).head

    Seq.fill(count)(file)
  }
  val processor = new FastNLPProcessorWithSemanticRoles {
    annotate("This is a test.")
  }
  val texts = files.map { file =>
    file.getName -> getText(file)
  }.toMap
  // The program was finally killed with the smallest one.
  val fileNames = files.sortBy(_.length).map(_.getName)
  // Optionally

  val runtime = Runtime.getRuntime
  println(s"TotalMemory: ${runtime.totalMemory}")
  println(s"  MaxMemory: ${runtime.maxMemory}")
  println(s"  MAX_VALUE: ${Long.MaxValue}")
  val scanner = new Scanner(System.in)

  def extractFrom(fileName: String): Unit = {
    // Include a self loop.
    println(s"Extracting from ${fileName}")
    println(s" FreeMemory: ${runtime.freeMemory}")
    val toText = texts(fileName)
    processor.annotate(toText, true)
  }

  val zipFiles = ThreadUtils.parallelize(fileNames.zipWithIndex, threads)
  while (true) {
    zipFiles.foreach { case (fromFileName, fromIndex) =>
      extractFrom(fromFileName)
    }
  }
}
