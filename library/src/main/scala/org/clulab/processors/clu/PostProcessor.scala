package org.clulab.processors.clu

import java.util.regex.Pattern
import scala.collection.mutable

object PostProcessor {
  //
  // Patterns for post-processing corrections
  //
  val VERSUS_PATTERN = Pattern.compile("""(?i)^vs\.?$""")

  // Matches agricultural season short hands such as "2021DS" or "2021WS"
  val WET_OR_DRY_SEASON = Pattern.compile("""(?i)[0-9]+(ds|ws)""")

  /** POS tag corrections, in place */
  def postprocessPartOfSpeechTags2(words: Seq[String], tags: mutable.Seq[String]): mutable.Seq[String] = {

    // unigram patterns
    words.indices.foreach { index =>
      if (tags(index) != "CC" && VERSUS_PATTERN.matcher(words(index)).matches) {
        tags(index) = "CC" // "versus" seems like a CC to me. but maybe not...
      }

      if(WET_OR_DRY_SEASON.matcher(words(index)).matches) {
        tags(index) = "CD" // such years should be CDs because our grammars expect it
      }
    }

    // bigram patterns
    words.indices.dropRight(1).foreach { curr =>
      val next = curr + 1
      // "due" in "due to" must be a preposition
      if (words(curr).equalsIgnoreCase("due") && words(next).equalsIgnoreCase("to")) {
        tags(curr) = "IN"
      }

      // "fall" followed by a CD must be NN
      else if(words(curr).equalsIgnoreCase("fall") && tags(next).equals("CD")) {
        tags(curr) = "NN"
      }
    }

    tags
  }

  /** POS tag corrections */
  def postprocessPartOfSpeechTags1(words: Seq[String], tags: Seq[String]): Seq[String] = {
    val newTags = words.indices.map { index =>
      val word = words(index)
      val oldTag = tags(index)
      val newTag = {
        // unigram patterns
        if (VERSUS_PATTERN.matcher(word).matches)
          "CC" // "versus" seems like a CC to me. but maybe not...
        else if (WET_OR_DRY_SEASON.matcher(word).matches)
          "CD" // such years should be CDs because our grammars expect it
        // bigram patterns
        else if (word.equalsIgnoreCase("due")) {
          if (words.lift(index + 1).map(_.toLowerCase).contains("to")) "IN" // "due" in "due to" must be a preposition
          else oldTag
        }
        else if (word.equalsIgnoreCase("fall")) {
          if (tags.lift(index + 1).contains("CD")) "NN" // "fall" followed by a CD must be NN
          else oldTag
        }
        else oldTag
      }

      newTag
    }

    newTags
  }

  def postprocessPartOfSpeechTags(words: Seq[String], tags: Seq[String]): Seq[String] = {
    val result1 = postprocessPartOfSpeechTags1(words, tags)
    val result2 = postprocessPartOfSpeechTags2(words, mutable.Seq(tags: _*))

    if (result1 != result2)
      println("It went awry!")

    result1
  }
}
