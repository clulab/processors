package org.clulab.numeric

/**
  * Parses textual numbers, e.g., "twelve hundred", into numbers, e.g., "1200"
  */
object NumberParser {
  def parse(words: Seq[String]): Double = {
    // TODO (Marco)
    words.head.toDouble
  }
}
