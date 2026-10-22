package org.clulab.processors.apps

import org.clulab.utils.FileUtils

import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.util.Using

/** Organizes the hexatags datasets for incremental parsing (for SiPE) */
object GenerateIncrementalParsingDataset extends App {
  val EOT_TOKEN = "##" // special token to indicate that the text is incomplete
  val EOT_TAG = "eot" // special hexa tag to indicate that the text is complete
  val EOT = s"$EOT_TOKEN\t$EOT_TAG"

  Using.resource(new PrintWriter("output.txt")) { pw =>
    val lines = FileUtils.getLinesFromFile(args(0)).map(_.trim).iterator

    while (lines.hasNext) {
      val nonEmptyLines = lines.takeWhile(_.nonEmpty).toIndexedSeq
      generateIncompleteSentences(nonEmptyLines, pw)
    }
  }

  def generateIncompleteSentences(words: Seq[String], pw: PrintWriter): Unit = {
    for (end <- 1 to words.length) {
      for (i <- 0 until end)
        pw.println(words(i))
      if (end < words.length)
        pw.println(EOT)
      pw.println()
    }
  }
}
