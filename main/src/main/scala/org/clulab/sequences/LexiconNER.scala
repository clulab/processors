package org.clulab.sequences

import org.clulab.processors.Sentence
import org.clulab.struct.{EntityValidator, HashTrie}

import scala.collection.mutable.ArrayBuffer
import LexiconNER._
import org.slf4j.{Logger, LoggerFactory}

/**
  * Lexicon-based NER, which efficiently recognizes entities from large dictionaries
  * This is a cleaned-up version of the old RuleNER
  *
  * @param matchers A map of tries to be matched for each given category label
  *                 The order of the matchers is important: it indicates priority during ties (first has higher priority)
  * @param knownCaseInsensitives Set of single-token entity names that can be spelled using lower case, according to the KB(s)
  * @param useLemmas If true, tokens are matched using lemmas, otherwise using words
  *
  * Create a LexiconNER object using LexiconNER.apply() (not the c'tor, which is private).
  * Use is by calling the find() method on a single sentence.
  *
  * Author: mihais
  * Created: 5/11/15
  * Modified: 9/27/17 - Clean up from RuleNER into LexiconNER
  */
class LexiconNER private (
  val matchers:Array[(String, HashTrie)],
  val knownCaseInsensitives:Set[String],
  val useLemmas:Boolean,
  val entityValidator: EntityValidator) {

  def find(sentence:Sentence):Array[String] = {
    val seq = findLongestMatch(sentence)
    seq
  }

  def getTokens(sentence:Sentence):Array[String] = {
    if (useLemmas) {
      sentence.lemmas.get
    } else {
      sentence.words
    }
  }

  /**
    * Finds the longest match across all matchers.
    * This means that the longest match is always chosen, even if coming from a matcher with lower priority
    * Only ties are disambiguated according to the order provided in the constructor
    */
  def findLongestMatch(sentence:Sentence):Array[String] = {
    val tokens = getTokens(sentence)
    val caseInsensitiveWords = tokens.map(_.toLowerCase)

    var offset = 0
    val labels = new ArrayBuffer[String]()
    while(offset < tokens.length) {
      // stores the spans found by all matchers
      val spans = new Array[Int](matchers.length)

      // attempt to match each category at this offset
      for (i <- matchers.indices) {
        spans(i) = findAt(tokens, caseInsensitiveWords, matchers(i)._2, offset, entityValidator)
        // if(spans(i) > 0) println(s"Offset $offset: Matched span ${spans(i)} for matcher ${matchers(i)._1}")
      }

      // pick the longest match
      // solve ties by preferring earlier (higher priority) matchers
      var bestSpanOffset = -1
      var bestSpan = -1
      for(i <- matchers.indices) {
        if(spans(i) > bestSpan) {
          bestSpanOffset = i
          bestSpan = spans(i)
        }
      }

      // found something!
      if(bestSpanOffset != -1) {
        assert(bestSpan > 0)
        val label = matchers(bestSpanOffset)._1
        //println(s"MATCHED LABEL $label from $offset to ${offset + bestSpan} (exclusive)!")
        labels += "B-" + label
        for(i <- 1 until bestSpan) {
          labels += "I-" + label
        }
        offset += bestSpan
        //println(s"Will continue matching starting at $offset")
      } else {
        labels += OUTSIDE_LABEL
        offset += 1
      }
    }

    labels.toArray
  }

  private def findAt(seq:Array[String],
                     caseInsensitiveSeq:Array[String],
                     matcher:HashTrie,
                     offset:Int,
                     validator:EntityValidator):Int = {
    val span = if (matcher.caseInsensitive) {
      matcher.findAt(caseInsensitiveSeq, offset, Some(validator))
    } else {
      matcher.findAt(seq, offset, Some(validator))
    }
    span
  }

}

object LexiconNER {
  val logger: Logger = LoggerFactory.getLogger(classOf[LexiconNER])
  val OUTSIDE_LABEL: String = "O"

  def apply(): LexiconNER = {
    
  }

  /** Merges labels from src into dst, without overlapping any existing labels in dst */
  def mergeLabels(dst:Array[String], src:Array[String]) {
    assert(dst.length == src.length)

    var offset = 0
    while(offset < dst.length) {
      if(src(offset) != OUTSIDE_LABEL) {
        // no overlap allowed
        // if overlap is detected, the corresponding labels in src are discarded
        if(! overlap(dst, src, offset)) {
          dst(offset) = src(offset)
          offset += 1
          while(offset < src.length && src(offset).startsWith("I-")) {
            dst(offset) = src(offset)
            offset += 1
          }
        } else {
          // overlap found; just ignore the labels in src
          offset += 1
          while(offset < src.length && src(offset).startsWith("I-")) {
            offset += 1
          }
        }
      } else {
        // nothing to merge
        offset += 1
      }
    }
  }

  // Used by mergeLabels above
  private def overlap(dst:Array[String], src:Array[String], offset:Int):Boolean = {
    var position = offset
    if(dst(position) != OUTSIDE_LABEL) return true
    position += 1
    while(position < src.length && src(position).startsWith("I-")) {
      if(dst(position) != OUTSIDE_LABEL) return true
      position += 1
    }
    false
  }
}

