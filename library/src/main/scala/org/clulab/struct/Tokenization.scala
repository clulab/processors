package org.clulab.struct

// An alternative design would not use aligned arrays, but an array of structures.
case class WordTokenization(raw: String, startOffset: Int, endOffset: Int, word: String)

case class Tokenization(
  raw: Array[String],
  startOffsets: Array[Int],
  endOffsets: Array[Int],
  words: Array[String]
) {

  def reverse: Tokenization = {
    Tokenization(
      raw = raw.reverse,
      startOffsets = startOffsets.reverse,
      endOffsets = endOffsets.reverse,
      words = words.reverse
    )
  }
}
