package org.clulab.sequences

import org.clulab.processors.Sentence
import org.clulab.sequences.LexiconNER._
import org.clulab.struct.EntityValidator
import org.clulab.struct.BooleanHashTrie

/**
  * Lexicon-based NER, which efficiently recognizes entities from large dictionaries
  * Note: This is a cleaned-up version of the old RuleNER.
  * Create a LexiconNER object using LexiconNER.apply() (not the c'tor, which is private).
  * Use it by calling the find() method on a single sentence.
  * See org.clulab.processors.TextLexiconNER for usage examples.
  *
  * @param matchers A map of tries to be matched for each given category label
  *                 The order of the matchers is important: it indicates priority during ties (first has higher priority)
  * @param knownCaseInsensitives Set of single-token entity names that can be spelled using lower case, according to the KB(s)
  * @param useLemmas If true, tokens are matched using lemmas, otherwise using words
  *
  * Author: mihais
  * Created: 5/11/15
  * Modified: 9/27/17 - Clean up from RuleNER into LexiconNER
  */
@SerialVersionUID(1000L)  
class SeparatedLexiconNER(
    val matchers: Array[BooleanHashTrie],
    knownCaseInsensitives: Set[String],
    useLemmas: Boolean,
    val entityValidator: EntityValidator) extends LexiconNER(knownCaseInsensitives, useLemmas) {
  // The matchers were all created by LexiconNER.apply which can only have one value for
  // caseInsensitiveMatching, so all these matchers should have the same value for caseInsensitive.
  protected val requiresCaseInsensitiveWords: Boolean = matchers.exists(_.caseInsensitive)
  protected val requiresCaseSensitiveWords: Boolean = matchers.exists(!_.caseInsensitive)
  /**
    * Matches the lexicons against this sentence
    * @param sentence The input sentence
    * @return An array of BIO notations the store the outcome of the matches
    */
  def find(sentence: Sentence): Array[String] = {
    val seq = findLongestMatch(sentence)
    seq
  }

  def getLabels: Seq[String] = matchers.map(_.label)

  /**
    * Finds the longest match across all matchers.
    * This means that the longest match is always chosen, even if coming from a matcher with lower priority
    * Only ties are disambiguated according to the order provided in the constructor
    */
  protected def findLongestMatch(sentence: Sentence): Array[String] = {
    val tokens = getTokens(sentence)
    // This is only needed if at least one is case insensitive.
    val caseInsensitiveWords = if (requiresCaseInsensitiveWords) tokens.map(_.toLowerCase) else tokens
    val labels = new Array[String](tokens.length)
    var offset = 0

    def setNextLabel(label: String): Unit = {
      labels(offset) = label
      offset += 1
    }

    while (offset < tokens.length) {
      val (span, index) = findAt(tokens, caseInsensitiveWords, offset)

      if (span > 0) {
        if (contentfulSpan(sentence, offset, span) && // does this look like a valid entity span?
            entityValidator.validMatch(sentence, offset, offset + span)) { // domain-specific constraints on entities
          val bLabel =  matchers(index).bLabel
          val iLabel =  matchers(index).iLabel

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

  protected def findAt(seq: Array[String], caseInsensitiveSeq: Array[String], offset: Int): (Int, Int) = {
    def findAt(matcher: BooleanHashTrie): Int =
        matcher.findAt(if (matcher.caseInsensitive) caseInsensitiveSeq else seq, offset).length

    matchers.indices.foldLeft((0, -1)) { case ((bestSpan, bestIndex), index) =>
      val span = findAt(matchers(index))

      if (span > bestSpan) (span, index)
      else (bestSpan, bestIndex)
    }
  }
}




