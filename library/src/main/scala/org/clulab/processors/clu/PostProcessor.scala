package org.clulab.processors.clu

import org.clulab.processors.Sentence

import java.util.regex.Pattern
import org.clulab.struct.Edge

object PostProcessor {
  //
  // Patterns for post-processing corrections
  //
  val VERSUS_PATTERN = Pattern.compile("""(?i)^vs\.?$""")

  // Matches agricultural season short hands such as "2021DS" or "2021WS"
  val WET_OR_DRY_SEASON = Pattern.compile("""(?i)[0-9]+(ds|ws)""")

  /** POS tag corrections, in place */
  def postprocessPartOfSpeechTags(words: Seq[String], tags: Seq[String]): Seq[String] = {

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

}
