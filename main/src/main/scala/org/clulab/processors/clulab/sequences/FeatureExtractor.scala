package org.clulab.processors.clulab.sequences

import org.clulab.processors.Sentence

import scala.collection.mutable

/**
  * Implements common features used in sequence tagging
  * Created by mihais on 6/8/17.
  */
class FeatureExtractor(val sentence:Sentence, val position:Int, val features:mutable.HashSet[String]) {
  def word(offset:Int) {
    val i = position + offset
    if(validPosition(i))
      features += s"w[$offset]:${sentence.words(i)}"
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

  private def validPosition(i:Int):Boolean = {
    if(i >= 0 && i < sentence.size) true
    else false
  }
}
