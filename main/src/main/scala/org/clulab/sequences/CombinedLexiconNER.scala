package org.clulab.sequences

import org.clulab.processors.Sentence
import org.clulab.sequences.LexiconNER._
import org.clulab.struct.EntityValidator
import org.clulab.struct.IntHashTrie

/**
  * Lexicon-based NER, which efficiently recognizes entities from large dictionaries
  * Note: This is a cleaned-up version of the old SeparatedLexiconNER.
  * Create a LexiconNER object using LexiconNER.apply() (not the c'tor, which is private).
  * Use it by calling the find() method on a single sentence.
  * See org.clulab.processors.TextLexiconNER for usage examples.
  *
  * @param caseInsensitiveMatcher A map of tries to be matched for for case insensitive KBs
  * @param caseSensitiveMatcher A map of tries to be matched for for case sensitive KBs
  * @param knownCaseInsensitives Set of single-token entity names that can be spelled using lower case, according to the KB(s)
  * @param useLemmas If true, tokens are matched using lemmas, otherwise using words
  *
  * Author: mihais
  * Created: 5/11/15
  * Modified: 9/27/17 - Clean up from RuleNER into LexiconNER
  */
@SerialVersionUID(1000L)
class CombinedLexiconNER (
    val caseInsensitiveMatcher: IntHashTrie,
    val caseSensitiveMatcher: IntHashTrie,
    val labels: Seq[String],
    knownCaseInsensitives: Set[String],
    useLemmas: Boolean,
    val entityValidator: EntityValidator) extends LexiconNER(knownCaseInsensitives, useLemmas) {
  // Make sure they arrive in the right order.
  require(caseInsensitiveMatcher.caseInsensitive)
  require(!caseSensitiveMatcher.caseInsensitive)
  protected val hasCaseInsensitive: Boolean = caseInsensitiveMatcher.entriesSize > 0
  protected val hasCaseSensitive: Boolean = caseSensitiveMatcher.entriesSize > 0
  protected val bLabels: Seq[String] = labels.map("B-" + _)
  protected val iLabels: Seq[String] = labels.map("I-" + _)

  override def isCloseEnough(other: AnyRef): Boolean = {
    other.isInstanceOf[CombinedLexiconNER] && {
      val that = other.asInstanceOf[CombinedLexiconNER]

      super.isCloseEnough(that) &&
      this.caseInsensitiveMatcher.isCloseEnough(that.caseInsensitiveMatcher, labels) &&
      this.caseSensitiveMatcher.isCloseEnough(that.caseSensitiveMatcher,labels) &&
      this.entityValidator.isCloseEnough(that.entityValidator)
    }
  }

  def toString(stringBuilder: StringBuilder): Unit = {
    caseInsensitiveMatcher.toString(stringBuilder, labels)
    stringBuilder.append("\n")
    caseSensitiveMatcher.toString(stringBuilder, labels)
  }

  /**
    * Matches the lexicons against this sentence
    *
    * @param sentence The input sentence
    * @return An array of BIO notations the store the outcome of the matches
    */
  def find(sentence: Sentence): Array[String] = {
    val caseSensitiveTokens = getTokens(sentence)
    val caseInsensitiveTokens = if (hasCaseInsensitive) caseSensitiveTokens.map(_.toLowerCase) else caseSensitiveTokens
    val seq = findLongestMatch(sentence, caseSensitiveTokens, caseInsensitiveTokens)

    seq
  }

  def getLabels: Seq[String] = labels

  /**
    * Finds the longest match across all matchers.
    * This means that the longest match is always chosen, even if coming from a matcher with lower priority
    * Only ties are disambiguated according to the order provided in the constructor
    */
  protected def findLongestMatch(sentence: Sentence, caseSensitiveTokens: Array[String], caseInsensitiveTokens: Array[String]): Array[String] = {
    val labels = new Array[String](caseSensitiveTokens.length)
    val length = labels.length
    var offset = 0

    def setNextLabel(label: String): Unit = {
      labels(offset) = label
      offset += 1
    }

    def getSpanAndIndex: CombinedLexiconNER.SpanAndIndex = {

      def innerGetSpanAndIndex(condition: Boolean, intHashTrie: IntHashTrie, tokens: => Array[String]): CombinedLexiconNER.SpanAndIndex = {
        if (condition) {
          val intTrieNodeMatch = intHashTrie.findAt(tokens, offset)
          CombinedLexiconNER.SpanAndIndex(intTrieNodeMatch.length, intTrieNodeMatch.completePath)
        }
        else CombinedLexiconNER.noSpanAndIndex
      }

      // It may be excessive to search both each time, but it is done to match results
      // of the original version of the code.
      val caseInsensitiveSpanAndIndex = innerGetSpanAndIndex(hasCaseInsensitive, caseInsensitiveMatcher, caseInsensitiveTokens)
      val caseSensitiveSpanAndIndex   = innerGetSpanAndIndex(hasCaseSensitive,   caseSensitiveMatcher,   caseSensitiveTokens)

      caseInsensitiveSpanAndIndex.orBetter(caseSensitiveSpanAndIndex)
    }

    while (offset < length) {
      val CombinedLexiconNER.SpanAndIndex(span, index) = getSpanAndIndex

      if (span > 0) {
        if (contentfulSpan(sentence, offset, span) && // does this look like a valid entity span?
            entityValidator.validMatch(sentence, offset, offset + span)) { // domain-specific constraints on entities
          val bLabel = bLabels(index)
          val iLabel = iLabels(index)

          setNextLabel(bLabel)
          for (_ <- 1 until span)
            setNextLabel(iLabel)
        }
        else
          for (_ <- 0 until span)
            setNextLabel(OUTSIDE_LABEL)
      }
      else
        setNextLabel(OUTSIDE_LABEL)
    }
    labels
  }
}

object CombinedLexiconNER {

  case class SpanAndIndex(span: Int = 0, index: Int = -1) {

    def orBetter(other: SpanAndIndex): SpanAndIndex = {
      // Ties should never occur because the indexes in the two trees should not overlap.
      // Each KB is either case sensitive or not.  The same KB index can't be in both places.
      if (span != other.span)
        // We want the highest value for the span.
        if (span > other.span) this
        else other
      else
        // We want the lowest value for the index.
        if (index <= other.index) this
        else other
    }
  }

  protected val noSpanAndIndex: SpanAndIndex = SpanAndIndex()

  def apply(caseInsensitiveMatcher: IntHashTrie, caseSensitiveMatcher: IntHashTrie, labels: Seq[String],
      knownCaseInsensitives: Set[String], useLemmas: Boolean,entityValidator: EntityValidator):  CombinedLexiconNER =
    new  CombinedLexiconNER(caseInsensitiveMatcher, caseSensitiveMatcher, labels, knownCaseInsensitives,
        useLemmas, entityValidator)
}
