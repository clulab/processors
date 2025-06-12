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

  /** POS tag corrections */
  def postprocessPartOfSpeechTags(words: Seq[String], tags: Seq[String]): Seq[String] = {
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
}
