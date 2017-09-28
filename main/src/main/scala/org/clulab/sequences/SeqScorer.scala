package org.clulab.sequences

import scala.collection.mutable

/**
 * Computes P, R, F1 scores for the complete mentions produced by a sequence tagger, in the BIO notation
 * User: mihais
 * Date: 2/27/15
 */
class SeqScorer {
  class Counts(var total:Int = 0, var predicted:Int = 0, var correct:Int = 0) {
    def p = correct.toDouble / predicted.toDouble

    def r = correct.toDouble / total.toDouble

    def f1 = 2 * p * r / (p + r)
  }

  case class Mention(start:Int, end:Int, label:String)

  def score(outputs:List[List[(String, String)]]) {
    val counts = new mutable.HashMap[String, Counts]()
    for(sentence <- outputs) {
      scoreSentence(sentence, counts)
    }

    for(label <- counts.keySet) {
      val c = counts.get(label).get
      println(s"$label P:${c.p} R:${c.r} F1:${c.f1}")
    }
  }

  def mkMentions(sentence:Array[String]):Set[Mention] = {
    val mentions = new mutable.HashSet[Mention]()
    var offset = 0
    while(offset < sentence.size) {
      // found the start of a mention
      if(sentence(offset) != "O") {
        val start = offset
        val label = sentence(offset).substring(2)
        // traverse as long we have an I- for the same label
        offset += 1
        while(offset < sentence.size && sentence(offset).startsWith("I-") && sentence(offset).substring(2) == label) {
          offset += 1
        }
        val end = offset
        mentions.add(Mention(start, end, label))
      } else {
        offset += 1
      }
    }
    mentions.toSet
  }

  def getCounts(counts:mutable.Map[String, Counts], label:String):Counts = {
    if(! counts.contains(label)) {
      val c = new Counts()
      counts += label -> c
    }
    counts.get(label).get
  }

  def scoreSentence(sentence:List[(String, String)], counts:mutable.Map[String, Counts]) = {
    val goldMentions = mkMentions(sentence.map(_._1).toArray)
    val sysMentions = mkMentions(sentence.map(_._2).toArray)

    //println("Outputs: " + sentence)
    //println("Gold mentions: " + goldMentions)
    //println("Sys mentions: " + sysMentions)

    for(m <- goldMentions) {
      val c = getCounts(counts, m.label)
      c.total += 1
    }
    for(m <- sysMentions) {
      val c = getCounts(counts, m.label)
      c.predicted += 1
      if(goldMentions.contains(m))
        c.correct += 1
    }
  }
}
