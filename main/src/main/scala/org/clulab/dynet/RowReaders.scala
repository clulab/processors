/**
 * Task-specific readers for the Row class produced by ColumnReader
 * @author Mihai
 */

package org.clulab.dynet

import org.clulab.sequences.Row

import scala.collection.mutable.ArrayBuffer

class AnnotatedSentence(val words: IndexedSeq[String],
                        var posTags: Option[IndexedSeq[String]] = None,
                        var neTags: Option[IndexedSeq[String]] = None)

trait RowReader {
  def toAnnotatedSentence(rows: IndexedSeq[Row]): AnnotatedSentence

  def toLabels(rows: IndexedSeq[Row],
               predicateIndex: Option[Int] = None): IndexedSeq[String]

  def getWord(r: Row): String = r.get(0)
  def getLabel(r: Row): String = r.get(1)
}

class BasicRowReader extends RowReader {
  override def toAnnotatedSentence(sentence: IndexedSeq[Row]): AnnotatedSentence = {
    val words = sentence.map(getWord)
    new AnnotatedSentence(words)
  }

  override def toLabels(rows: IndexedSeq[Row],
                        predicateIndex: Option[Int]): IndexedSeq[String] =
    rows.map(getLabel)
}

class BasicRowReaderWithPosTags extends RowReader {
  def getPosTag(r: Row): String = r.get(3)

  override def toAnnotatedSentence(sentence: IndexedSeq[Row]): AnnotatedSentence = {
    val words = sentence.map(getWord)
    val tags = sentence.map(getPosTag)
    new AnnotatedSentence(words, Some(tags))
  }

  override def toLabels(rows: IndexedSeq[Row],
                        predicateIndex: Option[Int]): IndexedSeq[String] =
    rows.map(getLabel)
}

class SrlArgsRowReader extends RowReader {
  def getPosTag(r: Row): String = r.get(2) // use this carefully; this may not be available in all datasets!
  def getNeTag(r: Row): String = r.get(3) // use this carefully; this may not be available in all datasets!

  def getLabels(r: Row): IndexedSeq[String] = {
    val labels = new ArrayBuffer[String]()
    for (j <- SrlArgsRowReader.ARGS_START until r.length) {
      labels += r.get(j)
    }
    labels
  }

  def getPredicatePositions(rows: IndexedSeq[Row]): IndexedSeq[Int] =
    rows.map(getLabel).zipWithIndex.filter(_._1 == "B-P").map(_._2)

  override def toAnnotatedSentence(rows: IndexedSeq[Row]): AnnotatedSentence = {
    val words = rows.map(getWord)
    val posTags = rows.map(getPosTag)
    val neTags = rows.map(getNeTag)
    new AnnotatedSentence(words, Some(posTags), Some(neTags))
  }

  override def toLabels(rows: IndexedSeq[Row],
                        predicateIndex: Option[Int]): IndexedSeq[String] = {
    assert(predicateIndex.nonEmpty)
    assert(predicateIndex.get >= 0)
    rows.map(_.tokens(SrlArgsRowReader.ARGS_START + predicateIndex.get))
  }
}

object SrlArgsRowReader {
  val ARGS_START = 4 // column where SRL arguments begin
}
