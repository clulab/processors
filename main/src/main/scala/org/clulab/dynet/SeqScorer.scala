package org.clulab.dynet

import org.clulab.utils.MathUtils.round

import scala.collection.mutable

/**
 * Scores the labels assigned to a sequence of words
 * Unlike the CoNLL-2003 scorer, this scorer operates over individual tokens rather than entity spans
 */
object SeqScorer {
  val OUTSIDE_LABEL = "O"

  def f1(golds: IndexedSeq[String], preds: IndexedSeq[String]): ScoreCountsByLabel = {
    assert(golds.length == preds.length)
    val scoreCountsByLabel = new ScoreCountsByLabel
    for (e <- preds.zip(golds)) {
      // for accuracy
      scoreCountsByLabel.total += 1
      if(e._1 == e._2) {
        scoreCountsByLabel.correct += 1
      }

      // for recall
      if (e._2 != OUTSIDE_LABEL) {
        scoreCountsByLabel.incGold()
        scoreCountsByLabel.incGold(e._2)
      }

      // for precision
      if (e._1 != OUTSIDE_LABEL) {
        scoreCountsByLabel.incPredicted()
        scoreCountsByLabel.incPredicted(e._1)
        if (e._1 == e._2) {
          scoreCountsByLabel.incCorrect()
          scoreCountsByLabel.incCorrect(e._1)
        }
      }
    }

    scoreCountsByLabel
  }
}

case class ScoreCounts(var correct: Int = 0, var gold: Int = 0, var predicted: Int = 0)

class ScoreCountsByLabel {
  val map = new mutable.HashMap[String, ScoreCounts]
  var total = 0
  var correct = 0 // total and correct used to compute overall accuracy

  def labels: collection.Set[String] = map.keySet

  def incGold(label: String = "*", value: Int = 1): Unit = {
    val counts = map.getOrElseUpdate(label, ScoreCounts())
    counts.gold += value
  }

  def incPredicted(label: String = "*", value: Int = 1): Unit = {
    val counts = map.getOrElseUpdate(label, ScoreCounts())
    counts.predicted += value
  }

  def incCorrect(label: String = "*", value: Int = 1): Unit = {
    val counts = map.getOrElseUpdate(label, ScoreCounts())
    counts.correct += value
  }

  def incAll(counts: ScoreCountsByLabel): Unit = {
    correct += counts.correct
    total += counts.total

    for (label <- counts.map.keySet) {
      val c = counts.map(label)
      incGold(label, c.gold)
      incPredicted(label, c.predicted)
      incCorrect(label, c.correct)
    }
  }

  def precision(label: String = "*", decimals: Int = 2): Double = {
    val c = map(label).correct
    val p = map(label).predicted

    round(if (p != 0) c.toDouble / p else 0d, decimals)
  }

  def recall(label: String = "*", decimals: Int = 2): Double = {
    val c = map(label).correct
    val g = map(label).gold

    round(if (g != 0) c.toDouble / g else 0d, decimals)
  }

  def f1(label: String = "*", decimals: Int = 2): Double = {
    val p = precision(label, decimals = -1)
    val r = recall(label, decimals = -1)

    round(if (p != 0 && r != 0) 2.0 * p * r / (p + r) else 0d, decimals)
  }

  def accuracy(decimals: Int = 2): Double = {
    val a = correct.toDouble / total
    round(a, decimals)
  }
}

