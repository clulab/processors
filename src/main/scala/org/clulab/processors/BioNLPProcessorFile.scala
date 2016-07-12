package org.clulab.processors

import java.io.PrintWriter

import org.clulab.processors.bionlp.BioNLPProcessor

/**
 * Runs BioNLPProcessor on a single text file. The output is serialized in a .ser output file.
 * User: mihais
 * Date: 12/8/14
 */
object BioNLPProcessorFile {
  def main(args:Array[String]) {

    val proc = new BioNLPProcessor()
    val ser = new DocumentSerializer

    for (inputFile <- args){

      val outputFile = inputFile + ".ser"

      val content = io.Source.fromFile(inputFile).getLines().mkString("\n")
      println(s"Processing file with name ${inputFile}, containing ${content.size} characters...")
      val doc = proc.annotate(content)

      val os = new PrintWriter(outputFile)
      ser.save(doc, os)
      os.close()
    }
  }
}
