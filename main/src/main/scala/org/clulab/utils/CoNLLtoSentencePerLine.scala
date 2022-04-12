package org.clulab.utils

import java.io.PrintWriter

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Converts the CoNLL format into the one-sentence-per-line required by our LMs
 * This produces the same format as the 1-billion-word-language-modeling-benchmark-r13output dataset
 *
 * @author Mihai
 */
object CoNLLtoSentencePerLine {
  def main(args: Array[String]): Unit = {
    assert(args.length == 2)
    val source = Source.fromFile(args(0))
    val dest = new PrintWriter(args(1))

    var words = new ArrayBuffer[String]()
    var sentCount = 0
    for(line <- source.getLines()) {
      val tokens = line.split("\\s+")
      if(tokens.nonEmpty) {
        words += tokens(0) // the first token must be the current word; we ignore all others
      } else {
        // reach end of a sentence
        dest.println(words.mkString(" "))
        words = new ArrayBuffer[String]()
        sentCount += 1
      }
    }
    if(words.nonEmpty) {
      dest.println(words.mkString(" "))
      sentCount += 1
    }

    println(s"Converted $sentCount sentences.")
    source.close()
    dest.close()
  }
}
