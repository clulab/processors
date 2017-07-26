package org.clulab.processors.clu.sequences

import org.clulab.processors.Sentence

import scala.collection.mutable

/**
  * Implements common features used in sequence tagging
  * Created by mihais on 6/8/17.
  */
class FeatureExtractor(
  val sentence:Sentence,
  val allowableLabels:mutable.HashMap[String, mutable.HashSet[String]],
  val position:Int,
  val features:mutable.HashSet[String]) {

  def word(offset:Int) {
    val i = position + offset
    if(i == -1)
      features += s"w[$offset]:-BOS-"
    else if(i == sentence.size)
      features += s"w[$offset]:-EOS-"
    else if(validPosition(i))
      features += s"w[$offset]:${sentence.words(i)}"
  }

  def wordBigrams(offset:Int) {
    val i = position + offset
    if(validPosition(i) && validPosition(i - 1)) {
      features += s"wb[$offset]:${sentence.words(i - 1)}-${sentence.words(i)}"
    }
  }

  def lemmaBigrams(offset:Int) {
    val i = position + offset
    if(validPosition(i) && validPosition(i - 1)) {
      features += s"wb[$offset]:${sentence.lemmas.get(i - 1)}-${sentence.lemmas.get(i)}"
    }
  }

  def lemma(offset:Int) {
    val i = position + offset
    if(validPosition(i))
      features += s"l[$offset]:${sentence.lemmas.get(i)}"
  }

  def casing(offset:Int) {
    val i = position + offset
    if(validPosition(i)) {
      var uppers = 0
      val w = sentence.words(i)
      for(j <- 0 until w.length) {
        if(Character.isUpperCase(w.charAt(j))) {
          uppers += 1
        }
      }

      var v = "x"
      if(uppers == w.length) v = "X"
      else if(uppers == 1 && Character.isUpperCase(w.charAt(0))) v = "Xx"
      else if(uppers >= 1 && ! Character.isUpperCase(w.charAt(0))) v = "xX"

      features += s"c[$offset]:$v"
    }
  }

  def suffixes(offset:Int, minLen:Int, maxLen:Int) {
    val i = position + offset
    if(validPosition(i)) {
      val w = sentence.words(i).toLowerCase()
      for(len <- minLen to maxLen) {
        if(len < w.length) {
          val suff = w.substring(w.length - len)
          features += s"suff[$offset,$len]:$suff"
        }
      }
    }
  }

  def allowable(offset:Int) {
    val i = position + offset
    if(validPosition(i)) {
      val w = sentence.words(i).toLowerCase
      if(allowableLabels.contains(w)) {
        val a = allowableLabels.get(w).get.toList.sorted.mkString("-")
        println(s"allowable($w) = $a")
        features += s"a[$offset]:$a"
      } else {
        features += s"a[$offset]:unk"
      }
    }
  }

  private def validPosition(i:Int):Boolean = {
    if(i >= 0 && i < sentence.size) true
    else false
  }
}
