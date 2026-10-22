package org.clulab.processors.apps

import org.clulab.utils.FileUtils

import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer

/** Organizes the hexatags datasets for incremental parsing (for SiPE) */
object GenerateIncrementalParsingDataset extends App {
  val EOT_TOKEN = "##" // special token to indicate that the text is incomplete
  val EOT_TAG = "eot" // special hexa tag to indicate that the text is complete

  val pw = new PrintWriter("output.txt")
  val lineBuffer = new ArrayBuffer[String]()
  for(line <- FileUtils.getLinesFromFile(args(0))) {
    if(line.trim.isEmpty) {
      generateIncompleteSentences(lineBuffer, pw)
      lineBuffer.clear()
    } else {
      lineBuffer += line.trim
    }
  }
  pw.close()

  def generateIncompleteSentences(words: ArrayBuffer[String], pw: PrintWriter): Unit = {
    for(end <- 1 to words.length) {
      for(i <- 0 until end) {
        pw.println(words(i))
      }
      if(end < words.length) {
        pw.println(s"$EOT_TOKEN\t$EOT_TAG")
      }
      pw.println()
    }
  }
}
