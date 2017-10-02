package org.clulab.processors.clu.sequences

import org.clulab.processors.Sentence
import org.clulab.sequences.SequenceTaggerLogger
import org.clulab.struct.Counter

/**
  * Implements common features used in sequence tagging
  * Created by mihais on 6/8/17.
  */
class FeatureExtractor(
  val sentence:Sentence,
  val position:Int,
  val features:Counter[String]) {

  def word(offset:Int) {
    val i = position + offset
    if(i == -1)
      features += s"w[$offset]:-BOS-"
    else if(i == sentence.size)
      features += s"w[$offset]:-EOS-"
    else if(validPosition(i))
      features += s"w[$offset]:${FeatureExtractor.norm(sentence.words(i))}"
  }

  def wordBigrams(offset:Int, threshold:Int) {
    val i = position + offset
    if(validPosition(i) && validPosition(i - 1)) {
      val bg = FeatureExtractor.mkBigram(sentence, i - 1)
      if(FeatureExtractor.bigrams.isEmpty || // during testing
         FeatureExtractor.bigrams.get.getCount(bg) > threshold) { // during training
        features += s"wb[$offset]:$bg"
      }
    }
  }

  def lemma(offset:Int) {
    val i = position + offset
    if(validPosition(i))
      features += s"l[$offset]:${FeatureExtractor.norm(sentence.lemmas.get(i))}"
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

  def features(offset:Int) {
    val i = position + offset
    if(validPosition(i)) {
      val w = sentence.words(i)
      var containsPeriod = false
      var containsNumber = false
      var containsHyphen = false
      var containsComma = false

      for (j <- w.indices) {
        val c = w.charAt(j)

        if(c == '-') containsHyphen = true
        else if(Character.isDigit(c)) containsNumber = true
        else if(c == '.') containsPeriod = true
        else if(c == ',') containsComma = true
      }

      if(containsPeriod) {
        features += s"hasPeriod[$offset]"
      }
      if(containsNumber) {
        features += s"hasNumber[$offset]"
      }
      if(containsHyphen) {
        features += s"hasHyphen[$offset]"
      }
      if(containsComma) {
        features += s"hasComma[$offset]"
      }
    }
  }

  def wordLen(offset:Int) {
    val i = position + offset
    if(validPosition(i)) {
      val w = sentence.words(i)
      features.setCount(s"wordLen[$offset]", w.length)
    }
  }

  def suffixes(offset:Int, minLen:Int, maxLen:Int) {
    val i = position + offset
    if(validPosition(i)) {
      val w = sentence.words(i).toLowerCase()
      for(len <- minLen to maxLen) {
        if(len <= w.length) {
          val suff = w.substring(w.length - len)
          features += s"suff[$offset,$len]:$suff"
        }
      }
    }
  }

  def prefixes(offset:Int, minLen:Int, maxLen:Int) {
    val i = position + offset
    if(validPosition(i)) {
      val w = sentence.words(i).toLowerCase()
      for(len <- minLen to maxLen) {
        if(len <= w.length) {
          val prefix = w.substring(0, len)
          features += s"pref[$offset,$len]:$prefix"
        }
      }
    }
  }

  def sentenceInfo() {
    val last = sentence.words.last
    features += s"eos:$last"
  }

  private def validPosition(i:Int):Boolean = {
    if(i >= 0 && i < sentence.size) true
    else false
  }
}

object FeatureExtractor {
  var bigrams:Option[Counter[String]] = None

  /**
    * Counts the bigrams seen in this corpus so we filter out the non-frequent ones
    * @param sentences The training corpus
    */
  def countBigrams(sentences:Seq[Sentence]): Unit = {
    SequenceTaggerLogger.logger.debug(s"Counting bigrams in ${sentences.size} sentences...")
    bigrams = Some(new Counter[String]())
    for(sentence <- sentences) {
      for(i <- 0 until sentence.size - 1) {
        bigrams.get += mkBigram(sentence, i)
      }
    }
    SequenceTaggerLogger.logger.debug(s"Found ${bigrams.get.size} unique bigrams.")
  }

  def norm(w:String):String = {
    w.replaceAll("\\d", "N")
  }

  def mkBigram(sentence:Sentence, i:Int):String =
    s"${norm(sentence.words(i).toLowerCase())}-${norm(sentence.words(i + 1).toLowerCase())}"
}
