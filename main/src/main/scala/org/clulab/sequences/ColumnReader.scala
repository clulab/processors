package org.clulab.sequences

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Reads the CoNLL-like column format
  */
object ColumnReader {
  def readColumns(fn: String): Array[Array[Row]] = {
    val source = Source.fromFile(fn)
    readColumns(source: Source)
  }

  def readColumns(source: Source): Array[Array[Row]] = {
    var sentence = new ArrayBuffer[Row]()
    val sentences = new ArrayBuffer[Array[Row]]()
    for (line <- source.getLines()) {
      val l = line.trim
      if (l.isEmpty) {
        // end of sentence
        if (sentence.nonEmpty) {
          sentences += sentence.toArray
          sentence = new ArrayBuffer[Row]
        }
      } else {
        // within the same sentence
        val bits = l.split("\\s")
        if (bits.length < 2)
          throw new RuntimeException(s"ERROR: invalid line [$l]!")
        sentence += Row(bits)
      }
    }

    if (sentence.nonEmpty) {
      sentences += sentence.toArray
    }

    source.close()
    sentences.toArray
  }
}

case class Row(tokens:Array[String]) {
  def get(idx:Int): String =
    if(idx < tokens.length) tokens(idx)
    else ""

  def getWord: String = get(0)
  def getTag: String = get(1)
  def getPos: String = get(2)

  def length = tokens.length
}

object Row {
  val ARG_START = 3
}

