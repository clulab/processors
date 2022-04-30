package org.clulab.learning

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

/**
 * Evaluates the output of svm_rank_classify or other reranking classifier
 * This currently computes P@1
 * This works for the QA system, but it should work for every generic svm_rank_classify like output
 * User: mihais
 * Date: 9/11/13
 */
class Answer(val gold:Double, var score:Double)
class Qid(val answers:Array[Answer])

object RankerEvaluator {

  def main(args:Array[String]): Unit = {
    if(args.length != 2) {
      println("Usage: org.clulab.qa.EvalSVMRank <gold> <predictions>")
      System.exit(1)
    }

    val goldFile = args(0)
    val outputFile = args(1)

    scoreSVMRank(goldFile, outputFile)
  }

  /**
   * Computes P@1 scores for data in svm_rank format
   * @param goldFile The test corpus (svm_rank format) with gold labels
   * @param outputFile Scores of the ranker being evaluated
   * @return The P@1 score
   */
  def scoreSVMRank(goldFile:String, outputFile:String):Double = {
    var scoresBuffer = new ArrayBuffer[Double]()
    for(line <- Source.fromFile(outputFile).getLines()) {
      val bits = line.split("\\s+")
      scoresBuffer += bits(0).toDouble
    }
    var scores = scoresBuffer.toArray

    var crtQid = ""
    val qidsBuffer = new ArrayBuffer[Qid]()
    var answers = new ArrayBuffer[Answer]()
    var offset = 0
    for(line <- Source.fromFile(goldFile).getLines()) {
      val bits = line.split("\\s+")
      val gold = bits(0)
      val qid = bits(1)

      if(qid != crtQid) {
        if(crtQid != "") {
          qidsBuffer += new Qid(answers.toArray)
        }
        crtQid = qid
        answers = new ArrayBuffer[Answer]()
      }
      answers += new Answer(gold.toDouble, scores(offset))
      offset += 1
    }
    if(crtQid != "") {
      qidsBuffer += new Qid(answers.toArray)
    }
    val qids = qidsBuffer.toArray
    println("Found " + qids.length + " questions.")


    for(qid <- qids) {
      for(a <- qid.answers) {
        println(a.gold + " " + a.score)
      }
      println()
    }

    println("Baseline scores:")
    score(qids)

    val sortedQids = new Array[Qid](qids.length)
    for(i <- 0 until qids.length) {
      sortedQids(i) = new Qid(qids(i).answers.toList.sortBy(0 - _.score).toArray)
    }

    println("System scores:")
    score(sortedQids)
  }

  def maxGold(answers:Array[Answer]):Double = {
    var max = Double.MinValue
    for(a <- answers) if(a.gold > max) max = a.gold
    max
  }

  def score(qids:Array[Qid]):Double = {
    var correct = 0
    var total = 0
    for(qid <- qids) {
      if(qid.answers(0).gold == maxGold(qid.answers))
        correct += 1
      total += 1
    }

    val p = correct.toDouble/total
    println("P@1: " + p)
    p
  }

  /** Computes the P@1 score for these datums */
  def score[F](queries:Iterable[Iterable[Datum[Int, F]]], scores:Array[Array[Double]]):Double = {
    val total = queries.size.toDouble
    var correct = 0
    var qi = 0
    for(query <- queries) {
      // what's the label of the highest predicted score for this block?
      var bestScore = Double.MinValue
      var bestLabel = -1
      var i = 0
      for(d <- query) {
        if(scores(qi)(i) > bestScore) {
          bestScore = scores(qi)(i)
          bestLabel = d.label
        }
        i += 1
      }

      // what's the best gold label for this block?
      var maxGold = Int.MinValue
      for(d <- query) {
        if(d.label > maxGold) {
          maxGold = d.label
        }
      }

      if(bestLabel == maxGold) {
        correct += 1
      }
      qi += 1
    }
    val p = correct.toDouble / total
    p
  }
}
