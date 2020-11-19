package org.clulab

import java.io._
import java.util.Scanner

import org.clulab.processors.fastnlp.FastNLPProcessorWithSemanticRoles

import scala.io.Source
import scala.util.Random

object ExtractSemanticRolesRandom extends App {

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

  val files = findFiles(inputDir, extension)
  val processor = new FastNLPProcessorWithSemanticRoles {
    annotate("This is a test.")
  }
  val texts = files.map { file =>
    file.getName -> getText(file)
  }.toMap
  val fileNames = files.sortBy(_.length).map(_.getName)

  val runtime = Runtime.getRuntime
  println(s"TotalMemory: ${runtime.totalMemory}")
  println(s"  MaxMemory: ${runtime.maxMemory}")
  println(s"  MAX_VALUE: ${Long.MaxValue}")
  val scanner = new Scanner(System.in)

  var seed = 0
  while (true) {
    println(s"Seed is $seed")
//    scanner.nextLine()
    val random = new Random(seed)
    seed += 1
    Range(0, 1).foreach { _ => // Give it 3 chances to mess up.
      val shuffledNames = random.shuffle(fileNames)
      shuffledNames.par.foreach { fileName =>
        println(s"Extracting from ${fileName}")
        println(s" FreeMemory: ${runtime.freeMemory}")

        val text = texts(fileName)
        processor.annotate(text, true)
      }
    }
  }
}
