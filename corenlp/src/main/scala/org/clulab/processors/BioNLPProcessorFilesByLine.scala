package org.clulab.processors

import java.io.{File, PrintWriter}

import scala.io.Source
import scala.collection.mutable.ListBuffer

import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.serialization.DocumentSerializer
import org.clulab.utils.Files

/**
 * Runs BioNLPProcessor on a bunch of files, where each file stores one sentence per line
 * User: mihais
 * Date: 12/5/14
 * Last Modified: Fix compiler issue: import scala.io.Source.
 */
object BioNLPProcessorFilesByLine {
  val EXT = "sents" // extension of files to be processed

  def main(args:Array[String]) {
    val files = Files.findFiles(args(0), EXT)
    println(s"Found ${files.size} files to be processed.")

    val proc = new BioNLPProcessor()
    val ser = new DocumentSerializer
    var count = 0
    for(file <- files) {
      println(s"Starting to process file $file...")
      val sents = fileToSentences(file)
      val doc = proc.annotateFromSentences(sents)
      val os = new PrintWriter(file + ".ser")
      ser.save(doc, os)
      os.close()

      println("Saved serialized file " + file + ".ser")
      count += 1
      println(s"Processed $count/${files.size} files.")
    }
  }

  def fileToSentences(file:File):List[String] = {
    val sents = new ListBuffer[String]
    Source.fromFile(file).getLines().foreach(sents += _)
    sents.toList
  }
}
