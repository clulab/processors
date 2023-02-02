/**
 * Task-specific readers for the Row class produced by ColumnReader
 * @author Mihai
 */

package org.clulab.dynet

import org.clulab.scala.WrappedArray._
import org.clulab.scala.WrappedArrayBuffer._
import org.clulab.sequences.Row
import org.clulab.utils.{Buffer, MathUtils}

import scala.collection.mutable
import scala.util.Random
import MetalRowReader._

case class AnnotatedSentence(words: IndexedSeq[String],
                             posTags: Option[IndexedSeq[String]] = None,
                             neTags: Option[IndexedSeq[String]] = None) {
  def indices: Range = words.indices
  def size: Int = words.size
}

trait RowReader {
  /** Converts the tabular format into one or more (AnnotatedSentence, sequence of gold heads (optional), sequence of gold labels) pairs */
  def toAnnotatedSentences(rows: IndexedSeq[Row], insertNegatives: Int = 0): IndexedSeq[(AnnotatedSentence, IndexedSeq[Label])]
}

class MetalRowReader extends RowReader {
  override def toAnnotatedSentences(rows: IndexedSeq[Row], insertNegatives: Int = 0): IndexedSeq[(AnnotatedSentence, IndexedSeq[Label])] = {

    if (rows.head.length == 2) {
      parseSimple(rows)
    } else if (rows.head.length == 4) {
      parseSimpleExtended(rows)
    } else if (rows.head.length >= 5) {
      parseFull(rows, insertNegatives)
    } else {
      throw new RuntimeException("ERROR: the Metal format expects 2, 4, or 5+ columns!")
    }
  }

  /** Parser for the simple format: word, label */
  def parseSimple(rows: IndexedSeq[Row]): IndexedSeq[(AnnotatedSentence, IndexedSeq[Label])] = {
    assert(rows.head.length == 2)
    val words = rows.map(_.get(WORD_POSITION))
    val labels = rows.map(row => PrimalLabel(row.get(WORD_POSITION + 1)))

    IndexedSeq((AnnotatedSentence(words), labels))
  }

  /** Parser for the simple extended format: word, POS tag, NE label, label */
  def parseSimpleExtended(rows: IndexedSeq[Row]): IndexedSeq[(AnnotatedSentence, IndexedSeq[Label])] = {
    assert(rows.head.length == 4)
    val words = rows.map(_.get(WORD_POSITION))
    val posTags = rows.map(_.get(POS_TAG_POSITION))
    val neLabels = rows.map(_.get(NE_LABEL_POSITION))
    val labels = rows.map(row => PrimalLabel(row.get(LABEL_START_OFFSET)))

    IndexedSeq(Tuple2(AnnotatedSentence(words, Some(posTags), Some(neLabels)), labels))
  }

  /** Parser for the full format: word, POS tag, NE label, (label head)+ */
  def parseFull(rows: IndexedSeq[Row], insertNegatives: Int): IndexedSeq[(AnnotatedSentence, IndexedSeq[Label])] = {
    assert(rows.head.length >= 5)
    val numSent = (rows.head.length - 3) / 2
    assert(numSent >= 1)

    val words = rows.map(_.get(WORD_POSITION))
    val posTags = rows.map(_.get(POS_TAG_POSITION))
    val neLabels = rows.map(_.get(NE_LABEL_POSITION))
    val labels = IndexedSeq.tabulate(numSent) { index =>
      rows.map(_.get(LABEL_START_OFFSET + (index * 2)))
    }
    val headPositions = IndexedSeq.tabulate(numSent) { index =>
      rows.map { row =>
        try {
          row.get(LABEL_START_OFFSET + (index * 2) + 1).toInt
        }
        catch {
          case _: NumberFormatException => -1
        }
      }
    }

    val annotatedSent = AnnotatedSentence(
      words,
      Some(posTags),
      Some(neLabels)
    )
    val sentences = labels.zip(headPositions).map { case (labelsForThisSentence, headsForThisSentence) =>
      val sentLabels = Buffer.makeArray[Label] { sentLabelsBuffer =>
        for (j <- labelsForThisSentence.indices) {
          sentLabelsBuffer += DualLabel(j, headsForThisSentence(j), labelsForThisSentence(j))

          if (insertNegatives > 0) {
            val negHeads = mkRandoms(-1 until annotatedSent.size, Set(headsForThisSentence(j)), insertNegatives)
            for (negHead <- negHeads) {
              sentLabelsBuffer += DualLabel(j, negHead, Utils.STOP_TAG)
            }
          }
        }
      }

      (annotatedSent, sentLabels: IndexedSeq[Label])
    }

    sentences
  }
}

object MetalRowReader {
  val WORD_POSITION = 0
  val POS_TAG_POSITION = 1
  val NE_LABEL_POSITION = 2
  val LABEL_START_OFFSET = 3

  val rand = new Random(1)

  private def mkRandoms(range: Range, exclude: Set[Int], howMany:Int): Set[Int] = {
    val numbers = MathUtils.randomize(range.toArray, rand)
    val randoms = new mutable.HashSet[Int]()
    for(n <- numbers if randoms.size < howMany) {
      if(! exclude.contains(n)) {
        randoms += n
      }
    }
    randoms.toSet
  }
}