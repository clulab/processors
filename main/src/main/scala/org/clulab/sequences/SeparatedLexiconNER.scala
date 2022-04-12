package org.clulab.sequences

import org.clulab.processors.Sentence
import org.clulab.sequences.LexiconNER._
import org.clulab.struct.EntityValidator
import org.clulab.struct.BooleanHashTrie

/**
  * Lexicon-based NER, which efficiently recognizes entities from large dictionaries
  *
  * Note: This is a cleaned-up version of the old RuleNER.  It may have been known
  * simply as LexiconNER at one point, but was renamed to emphasize the fact that each
  * KB is stored in a separate matcher ([[org.clulab.struct.BooleanHashTrie BooleanHashTrie]]).
  * Other variations get by with fewer matchers.
  *
  * Create a SeparatedLexiconNER object using either LexiconNER.apply() or
  * [[org.clulab.sequences.SlowLexiconNERBuilder#build SlowLexiconNERBuilder.build()]] rather
  * than by the constructor if at all possible.  Use it by calling the find() method on a
  * single sentence.
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

  override def equalsForSerialization(other: AnyRef): Boolean = {
    other.isInstanceOf[SeparatedLexiconNER] && {
      val that = other.asInstanceOf[SeparatedLexiconNER]

      super.equalsForSerialization(that) &&
      this.matchers.zip(that.matchers).forall { case (thisMatcher, thatMatcher) => thisMatcher.equalsForSerialization(thatMatcher) }
      this.entityValidator.equalsForSerialization(that.entityValidator)
    }
  }

  def toString(stringBuilder: StringBuilder): Unit = {
    if (matchers.size > 0) {
      stringBuilder.append("\n")

      matchers.zipWithIndex.foreach { case (matcher: BooleanHashTrie, index: Int) =>
        if (index > 0)
          stringBuilder.append("\n")
        matcher.toString(stringBuilder)
      }
      stringBuilder.append("\n")
    }
  }

  /**
    * Matches the lexicons against this sentence
    * @param sentence The input sentence
    * @return An array of BIO notations the store the outcome of the matches
    */
  def find(sentence: Sentence): Array[String] = {
    val seq = findLongestMatch(sentence)
    seq
  }

  def getLabels: Seq[String] = matchers.map(_.label).distinct

  /**
    * Finds the longest match across all matchers.
    * This means that the longest match is always chosen, even if coming from a matcher with lower priority
    * Only ties are disambiguated according to the order provided in the constructor
    */
  protected def findLongestMatch(sentence: Sentence): Array[String] = {
    val tokens = getTokens(sentence)
    // Calculate this just once here for all matchers.
    val caseInsensitiveTokens = tokens.map(_.toLowerCase)
    val labels = new Array[String](tokens.length)
    var offset = 0

    def setNextLabel(label: String): Unit = {
      labels(offset) = label
      offset += 1
    }

    while (offset < tokens.length) {
      val (span, index) = findAt(tokens, caseInsensitiveTokens, offset)

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

  protected def findAt(tokens: Array[String], caseInsensitiveTokens: Array[String], offset: Int): (Int, Int) = {
    def findAt(matcher: BooleanHashTrie): Int =
        matcher.findAt(if (matcher.caseInsensitive) caseInsensitiveTokens else tokens, offset).length

    matchers.indices.foldLeft((0, -1)) { case ((bestSpan, bestIndex), index) =>
      val span = findAt(matchers(index))

      if (span > bestSpan) (span, index)
      else (bestSpan, bestIndex)
    }
  }
}
