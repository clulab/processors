package org.clulab.processors.clu

import org.clulab.processors.Sentence

import java.util.regex.Pattern
import org.clulab.struct.Edge

object PostProcessor {
  //
  // Patterns for post-processing corrections
  //
  val VERSUS_PATTERN = Pattern.compile("""(?i)^vs\.?$""")

  /** POS tag corrections, in place */
  def postprocessPartOfSpeechTags(words: Array[String], tags: Array[String]): Array[String] = {

    // unigram patterns
    words.indices.foreach { index =>
      if (tags(index) != "CC" && VERSUS_PATTERN.matcher(words(index)).matches) {
        tags(index) = "CC" // "versus" seems like a CC to me. but maybe not...
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
