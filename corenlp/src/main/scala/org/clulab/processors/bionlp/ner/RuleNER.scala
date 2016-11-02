package org.clulab.processors.bionlp.ner

import org.clulab.processors.Sentence
import org.clulab.struct.{EntityValidator, HashTrie}
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer

import RuleNER._

/**
 * Rule-based NER for the Bio domain
 * If useLemmas is true, tokens are matched using lemmas, otherwise using words
 * knownCaseInsensitives contains single-token entities that can be spelled using lower case, according to the KB(s)
 * The order of the matchers is important: it indicates priority during ties (first has higher priority)
 * User: mihais
 * Date: 5/11/15
 */
class RuleNER(val matchers:Array[(String, HashTrie)], val knownCaseInsensitives:Set[String], val useLemmas:Boolean = false) {

  def this(matchers:Array[(String, HashTrie)], useLemmas:Boolean) { this(matchers, Set[String](), useLemmas) }

  def find(sentence:Sentence):Array[String] = {
    // findByPriority(sentence)
    val seq = findLongestMatch(sentence)
    seq
  }

  def getTokens(sentence:Sentence):Array[String] = {
    useLemmas match {
      case true => sentence.lemmas.get
      case _ => sentence.words
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
    val validator = new RuleEntityValidator(sentence, knownCaseInsensitives)

    var offset = 0
    val labels = new ArrayBuffer[String]()
    while(offset < tokens.length) {
      // stores the spans found by all matchers
      val spans = new Array[Int](matchers.length)

      // attempt to match each category at this offset
      for (i <- matchers.indices) {
        spans(i) = findAt(tokens, caseInsensitiveWords, matchers(i)._2, offset, validator)
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
    val span = matcher.caseInsensitive match {
      case true => matcher.findAt(caseInsensitiveSeq, offset, Some(validator))
      case _ => matcher.findAt(seq, offset, Some(validator))
    }
    span
  }

  /**
   * Inspects matchers in the order provided in the constructor
   * This means that a matcher with higher priority is preferred even if a longer one (with lower priority) exists!
   */
  def findByPriority(sentence:Sentence):Array[String] = {
    val overallLabels = new Array[String](sentence.size)
    for(i <- overallLabels.indices) overallLabels(i) = OUTSIDE_LABEL
    val tokens = getTokens(sentence)
    for(matcher <- matchers) {
      // the actual match
      var labels = matcher._2.find(tokens, matcher._1, OUTSIDE_LABEL)

      // some matchers are overmatching due to the case-insensitive setting,
      // e.g., Gene_or_gene_product contains protein names that are identical to prepositions, such as "IN" and "TO"
      labels = filterMatches(labels, sentence)
      //println("LABELS: " + labels.mkString(" "))

      // matchers must be stored in descending order of their priorities
      // so we do not allow new labels to overwrite labels already generated
      RuleNER.mergeLabels(overallLabels, labels)
    }
    overallLabels
  }

  def filterMatches(labels:Array[String], sentence:Sentence):Array[String] = {
    val filtered = removeSinglePrepositions(labels, sentence)

    filtered
  }

  /** Remove single tokens that are not tagged as nouns */
  def removeSinglePrepositions(labels:Array[String], sentence:Sentence):Array[String] = {
    val filtered = new Array[String](labels.length)
    for(i <- labels.indices) {
      if(labels(i).startsWith("B-") &&
         (i == labels.length - 1 || ! labels(i + 1).startsWith("I-")) && // single token entity
         ! sentence.tags.get(i).startsWith("NN")) { // not a noun
        filtered(i) = RuleNER.OUTSIDE_LABEL
      } else {
        filtered(i) = labels(i)
      }
    }
    filtered
  }

}

object RuleNER {
  val logger = LoggerFactory.getLogger(classOf[RuleNER])
  val OUTSIDE_LABEL = "O"

  /** Merges labels in src into dst, without overlapping any existing labels in dst */
  def mergeLabels(dst:Array[String], src:Array[String]) {
    assert(dst.length == src.length)

    var offset = 0
    while(offset < dst.length) {
      if(src(offset) != OUTSIDE_LABEL) {
        // no overlap allowed
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

  def overlap(dst:Array[String], src:Array[String], offset:Int):Boolean = {
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
