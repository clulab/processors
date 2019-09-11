package org.clulab.processors.bio

import java.util.regex.Pattern

import org.clulab.processors.Sentence
import org.clulab.processors.clu.SentencePostProcessor
import org.clulab.struct.MutableNumber
import org.clulab.utils.Files.loadStreamFromClasspath

import scala.collection.mutable
import BioNERPostProcessor._
import org.clulab.sequences.LexiconNER

/**
  *
  * User: mihais
  * Date: 10/22/17
  */
class BioNERPostProcessor(val stopWordFile:String) extends SentencePostProcessor {
  val stopWords: Set[String] = loadEntityStopList(stopWordFile)

  override def process(sent: Sentence): Unit = {
    val seq = sent.entities.get
    val tags = sent.tags.get
    val lemmas = sent.lemmas.get
    val words = sent.words

    //
    // This code *transforms* entity labels, e.g., by shortening entity spans
    //

    //
    // IF: "A ,(optional) and B complex" THEN: make sure "complex" is labeled "O"
    // This pattern is considered a Binding event, and will be modeled by REACH
    //
    var i = 0
    while(i < seq.length) {
      val offset = new MutableNumber[Int](i - 1)
      if(lemmas(i) == "complex" &&
        isEntity(offset, seq) &&
        isCC(offset, tags) &&
        isEntity(offset, seq)) {
        seq(i) = LexiconNER.OUTSIDE_LABEL
        seq(findEntityStart(i - 1, seq) - 1) = LexiconNER.OUTSIDE_LABEL
      }
      i += 1
    }

    //
    // This code *validates* entire entity spans as valid/invalid entity names
    //

    i = 0
    while(i < seq.length) {
      if(isEntityStart(i, seq)) {
        val end = findEntityEnd(i, seq)
        if(! validMatch(sent, i, end)) {
          for(j <- i until end) {
            // if no bueno, reset the entire span labels to O
            seq(j) = LexiconNER.OUTSIDE_LABEL
          }
        }
        i = end
      } else {
        i += 1
      }
    }
  }

  private def validMatch(sentence: Sentence, start: Int, end: Int):Boolean = {
    assert(end > start)
    val words = sentence.words
    val lemmas = sentence.lemmas.get

    //
    // single-character entities cannot exist in this domain
    //
    if(end - start == 1 && words(start).length <= 1) {
      return false
    }

    //
    // stop words should not be labeled when in lower case, or upper initial
    //
    if(end - start == 1 &&
      (isLowerCase(words(start)) || isUpperInitial(words(start))) &&
      stopWords.contains(words(start).toLowerCase)) {
      return false
    }

    //
    // XML tag leftovers should not be labeled
    //
    for(i <- start until end) {
      if(words(i).startsWith("XREF_")) {
        return false
      }
    }

    //
    // figure references, e.g., "Figure S2", should not be labeled
    //
    for(i <- start until end) {
      if(i > 0 && isFigRef(lemmas, i)) {
        return false
      }
    }

    true
  }

  private def isLowerCase(s:String):Boolean = {
    for(i <- 0 until s.length) {
      val c = s.charAt(i)
      if(Character.isLetter(c) && ! Character.isLowerCase(c))
        return false
    }
    true
  }

  private def isUpperInitial(s:String):Boolean = {
    if(s.length < 1) return false
    if(Character.isLetter(s.charAt(0)) && ! Character.isUpperCase(s.charAt(0)))
      return false

    for(i <- 1 until s.length) {
      val c = s.charAt(i)
      if(Character.isLetter(c) && ! Character.isLowerCase(c))
        return false
    }
    true
  }

  private def isFigRef(lemmas:Array[String], offset:Int):Boolean = {
    assert(offset > 0)
    val m1 = POTENTIAL_FIGURE_TEXT.matcher(lemmas(offset - 1))
    val m2 = POTENTIAL_FIGURE_NUMBER.matcher(lemmas(offset))
    if(m1.matches() && m2.matches())
      return true
    false
  }

  private def isEntity(offset:MutableNumber[Int], seq:Array[String]):Boolean = {
    if(offset.value >= 0 && (seq(offset.value).startsWith("B-") || seq(offset.value).startsWith("I-"))) {
      offset.value = findEntityStart(offset.value, seq) - 1
      return true
    }
    false
  }

  private def findEntityStart(offset:Int, seq:Array[String]):Int = {
    var i = offset
    while(i > 0 && seq(i).startsWith("I-"))
      i -= 1
    i
  }

  private def isEntityStart(offset:Int, seq:Array[String]):Boolean = {
    if(seq(offset).startsWith("B-")) return true
    // allow entities to start with "I-", in case the sequence tagger screwed up
    if(seq(offset).startsWith("I-")) return true
    false
  }

  private def findEntityEnd(offset:Int, seq:Array[String]):Int = {
    var i = offset
    if(seq(i).startsWith("B-"))
      i += 1
    while(i < seq.length && seq(i).startsWith("I-"))
      i += 1
    i
  }

  private def isCC(offset:MutableNumber[Int], tags:Array[String]):Boolean = {
    if(offset.value >= 0 && tags(offset.value) == "CC") {
      offset.value -= 1
      if(offset.value >= 0 && tags(offset.value) == ",")
        offset.value -= 1
      return true
    }
    false
  }
}

object BioNERPostProcessor {
  private val POTENTIAL_FIGURE_NUMBER = Pattern.compile("[a-z]*\\d+", Pattern.CASE_INSENSITIVE)
  private val POTENTIAL_FIGURE_TEXT = Pattern.compile("(figure|figures|fig\\.?|figs\\.?)", Pattern.CASE_INSENSITIVE)

  def loadEntityStopList(kb:String):Set[String] = {
    val stops = new mutable.HashSet[String]()
    val reader = loadStreamFromClasspath(kb)
    var done = false
    while(! done) {
      val line = reader.readLine()
      if(line == null) {
        done = true
      } else {
        val l = line.trim
        if(! l.isEmpty && ! l.startsWith("#")) {
          stops += l
        }
      }
    }
    reader.close()
    stops.toSet
  }
}
