/**
 * Task-specific readers for the Row class produced by ColumnReader
 * @author Mihai
 */

package org.clulab.dynet

import org.clulab.sequences.Row

import scala.collection.mutable.ArrayBuffer

import MetalRowReader._

case class AnnotatedSentence(words: IndexedSeq[String],
                             posTags: Option[IndexedSeq[String]] = None,
                             neTags: Option[IndexedSeq[String]] = None,
                             headPositions: Option[IndexedSeq[Int]] = None) {
  def indices: Range = words.indices
  def size: Int = words.size
}

trait RowReader {
  /** Converts the tabular format into an AnnotatedSentence and a sequence of gold labels */
  def toAnnotatedSentence(rows: IndexedSeq[Row]): (AnnotatedSentence, IndexedSeq[String])
}

class MetalRowReader extends RowReader {
  override def toAnnotatedSentence(rows: IndexedSeq[Row]): (AnnotatedSentence, IndexedSeq[String]) = {
    val words = new ArrayBuffer[String]()
    val posTags = new ArrayBuffer[String]()
    val neLabels = new ArrayBuffer[String]()
    val headPositions = new ArrayBuffer[Int]()
    val labels = new ArrayBuffer[String]()

    for(i <- rows.indices) {
      if(rows(i).tokens.length == 2) {
        // simple format, containing just words and labels
        words += rows(i).get(WORD_POSITION)
        labels += rows(i).get(LABEL_POSITION)
      } else if(rows(i).tokens.length == 5) {
        // complete format, with all five columns
        words += rows(i).get(WORD_POSITION)
        labels += rows(i).get(LABEL_POSITION)
        posTags += rows(i).get(POS_TAG_POSITION)
        neLabels += rows(i).get(NE_POSITION)

        val head = try {
          rows(i).get(HEAD_POSITION).toInt
        } catch {
          case _: NumberFormatException => -1
        }
        headPositions += head
      } else {
        throw new RuntimeException("ERROR: the Metal dataset format expects either 2 or 5 columns!")
      }
    }

    val sentence =
      if(headPositions.isEmpty) {
        // simple format
        AnnotatedSentence(words, None, None, None)
      } else {
        // full format
        assert(headPositions.size == words.size)
        assert(posTags.size == words.size)
        assert(neLabels.size == words.size)
        AnnotatedSentence(words,
          Some(posTags),
          Some(neLabels),
          Some(headPositions))
      }

    (sentence, labels)
  }
}

object MetalRowReader {
  val WORD_POSITION = 0
  val LABEL_POSITION = 1
  val HEAD_POSITION = 2
  val POS_TAG_POSITION = 3
  val NE_POSITION = 4
}