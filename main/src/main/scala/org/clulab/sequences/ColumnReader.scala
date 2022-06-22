package org.clulab.sequences

import org.clulab.utils.Sourcer

import org.clulab.utils.Closer.AutoBufferedSource

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Reads the CoNLL-like column format
  */
object ColumnReader {
  def readColumns(fn: String): Array[Array[Row]] = {
    // That which opens the file should also close it, none other.
    Sourcer.sourceFromFilename(fn).autoClose { source =>
      readColumns(source: Source)
    }
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
        val bits = l.split("\\s+")
        if (bits.length < 2)
          throw new RuntimeException(s"ERROR: invalid line [$l]!")
        sentence += Row(bits)
      }
    }

    if (sentence.nonEmpty) {
      sentences += sentence.toArray
    }
    sentences.toArray
  }
}

/**
 * Stores training data for sequence modeling
 * Mandatory columns: 0 - word, 1 - label
 * Optional columns: 2 - POS tag, 3+ SRL arguments
 * @param tokens
 */
case class Row(tokens:Array[String]) {
  def get(idx:Int): String = {
    if(idx >= tokens.length) {
      throw new RuntimeException(s"ERROR: trying to read field #$idx, which does not exist in this row: [${tokens.mkString(", ")}]!")
    }
    tokens(idx)
  }

  def length = tokens.length
  def indices: Range = tokens.indices
}
