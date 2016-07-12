package org.clulab.discourse.rstparser

import org.clulab.struct.Counter

import scala.collection.mutable

/**
 * Scores predicted structures against a gold tree
 * User: mihais
 * Date: 5/24/14
 */
class DiscourseScorer {
  def score(sys:DiscourseTree,
            gold:DiscourseTree,
            score:DiscourseScore,
            scoreType:ScoreType.Value,
            verbose:Boolean = false) {
    if(verbose) {
      println(s"System tree:\n$sys")
      println(s"Gold tree:\n$gold")
    }

    val sysUnits = toDiscourseUnits(sys, scoreType)
    val goldUnits = toDiscourseUnits(gold, scoreType)

    if(verbose) {
      println("System units:")
      for (u <- sysUnits) println(u)
      println("Gold units:")
      for (u <- goldUnits) println(u)
    }

    score.total += goldUnits.size
    for(u <- goldUnits) score.incTotal(u.label)

    score.predicted += sysUnits.size
    for(u <- sysUnits) score.incPredicted(u.label)

    for(u <- sysUnits) {
      if(goldUnits.contains(u)) {
        score.correct += 1
        score.incCorrect(u.label)
      }
    }
  }

  def score(sysLabel:String,
            goldLabel:String,
            score:DiscourseScore) {
    //val (sl, sd) = StructureClassifier.parseLabel(sysLabel)
    //val (gl, gd) = StructureClassifier.parseLabel(goldLabel)
    val sl = sysLabel
    val gl = goldLabel

    score.total += 1
    score.incTotal(gl)
    score.predicted += 1
    score.incPredicted(sl)

    if(gl == sl) {
      score.correct += 1
      score.incCorrect(gl)
    }
  }

  def toDiscourseUnits(t:DiscourseTree, scoreType:ScoreType.Value):Set[DiscourseUnit] = {
    val units = new mutable.HashSet[DiscourseUnit]()
    addDiscourseUnits(t, units, scoreType)
    units.toSet
  }

  def addDiscourseUnits(t:DiscourseTree, units:mutable.HashSet[DiscourseUnit], scoreType:ScoreType.Value) {
    if(! t.isTerminal) {
      val labels = Array(StructureClassifier.POS, StructureClassifier.POS)
      if(scoreType == ScoreType.OnlyStructure) {
        // nothing to do; we're good
      } else if(scoreType == ScoreType.Full) {
        // use direction of the nuclearity to assign labels to children!
        if (t.relationDirection == RelationDirection.LeftToRight) {
          labels(0) = "span"
          labels(1) = t.relationLabel
        } else if (t.relationDirection == RelationDirection.RightToLeft) {
          labels(0) = t.relationLabel
          labels(1) = "span"
        } else {
          labels(0) = t.relationLabel
          labels(1) = t.relationLabel
        }
      } else {
        throw new RuntimeException("ERROR: unknown score type: " + scoreType)
      }
      assert(t.children.size == 2)
      for(i <- 0 until t.children.size) {
        val c = t.children(i)
        val l = labels(i)
        units += new DiscourseUnit(c.firstToken, c.lastToken, l)
      }

      for(c <- t.children) {
        addDiscourseUnits(c, units, scoreType)
      }
    }
  }
}

object ScoreType extends Enumeration {
  type ScoreType = Value
  val Full, OnlyStructure = Value
}

case class DiscourseUnit (firstToken:TokenOffset,
                          lastToken:TokenOffset,
                          label:String)

class DiscourseScore ( var total:Int = 0,
                       var predicted:Int = 0,
                       var correct:Int = 0,
                       val totalByLabel:Counter[String] = new Counter[String](),
                       val predictedByLabel:Counter[String] = new Counter[String](),
                       val correctByLabel:Counter[String] = new Counter[String]()) {

  def p:Double = correct.toDouble / predicted.toDouble

  def r:Double = correct.toDouble / total.toDouble

  def f1:Double = if(p != 0 && r != 0) 2.0 * p * r / (p + r) else 0

  def labels:Set[String] = {
    val ls = new mutable.HashSet[String]()
    ls ++= totalByLabel.keySet
    ls ++= predictedByLabel.keySet
    ls.toSet
  }

  def incTotal(label:String, v:Int = 1) { totalByLabel.incrementCount(label) }
  def incPredicted(label:String, v:Int = 1) { predictedByLabel.incrementCount(label) }
  def incCorrect(label:String, v:Int = 1) { correctByLabel.incrementCount(label) }

  def p(label:String):Double = correctByLabel.getCount(label) / predictedByLabel.getCount(label)

  def r(label:String):Double = correctByLabel.getCount(label) / totalByLabel.getCount(label)

  def f1(label:String):Double =
    if(p(label) != 0 && r(label) != 0) 2.0 * p(label) * r(label) / (p(label) + r(label)) else 0

  override def toString:String = {
    val os = new mutable.StringBuilder()
    val ls = labels

    if(ls.size > 1) {
      for(l <- ls) {
        os.append(s"$l\tP = ${p(l)}\tR = ${r(l)}\tF1 = ${f1(l)}\n")
      }
    }
    os.append(s"Total\tP = $p\tR = $r\tF1 = $f1\n")
    os.toString()
  }
}
