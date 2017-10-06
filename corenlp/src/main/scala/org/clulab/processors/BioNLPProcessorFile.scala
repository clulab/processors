package org.clulab.processors

import java.io.PrintWriter
import scala.io.Source

import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.serialization.DocumentSerializer

/**
 * Runs BioNLPProcessor on a single text file. The output is serialized in a .ser output file.
 * User: mihais
 * Date: 12/8/14
 * Last Modified: Fix compiler issue: import scala.io.Source.
 */
object BioNLPProcessorFile {
  def main(args:Array[String]) {

    val proc = new BioNLPProcessor()
    val ser = new DocumentSerializer

    for (inputFile <- args){

      val outputFile = inputFile + ".ser"

      val content = Source.fromFile(inputFile).getLines().mkString("\n")
      println(s"Processing file with name ${inputFile}, containing ${content.size} characters...")
      val doc = proc.annotate(content)

      val os = new PrintWriter(outputFile)
      ser.save(doc, os)
      os.close()
    }
  }
}
