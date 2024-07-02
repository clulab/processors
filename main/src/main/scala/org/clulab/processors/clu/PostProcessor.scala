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
  // def postprocessPartOfSpeechTags(words: Array[String], tags: Array[String]): Array[String] = tags

  def postprocessPartOfSpeechTags(words: Array[String], tags: Array[String]): Array[String] = {
    words.indices.foreach { index =>
      if (tags(index) != "CC" &&
          VERSUS_PATTERN.matcher(words(index)).matches)
        tags(index) = "CC" // "versus" seems like a CC to me. but maybe not...
    }
    words.indices.dropRight(1).foreach { curr =>
      val next = curr + 1
      // "due" in "due to" must be a preposition
      if (words(curr).equalsIgnoreCase("due") &&
          words(next).equalsIgnoreCase("to"))
        tags(curr) = "IN"
    }

    tags
  }

  /** 
   * Deterministic corrections for dependency parsing 
   * Modifies headsWithLabels in place
   */
  // def parserPostProcessing(sentence: Sentence, dependencies: Array[Dependency]): Unit = ()

  def parserPostProcessing(sentence: Sentence, dependencies: Array[Dependency]): Unit = {
    sentence.tags.foreach { tags => // Tags must be available.
      sentence.indices.dropRight(1).foreach { curr => // We're looking ahead 1.
        val next = curr + 1
        // "due to" must be a MWE
        if (sentence.words(curr).equalsIgnoreCase("due") && tags(curr) == "IN" &&
            sentence.words(next).equalsIgnoreCase("to")  && tags(next) == "TO")
          dependencies(next) = dependencies(next).copy(realHead = curr, label = "mwe")
      }
    }
  }

}
