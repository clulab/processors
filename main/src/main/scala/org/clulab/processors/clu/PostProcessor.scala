package org.clulab.processors.clu

import org.clulab.processors.Sentence

object PostProcessor {
  //
  // Patterns for post-processing corrections
  //
  val VERSUS_PATTERN = """(?i)^vs\.?$""".r

  /** POS tag corrections, in place */
  def postprocessPartOfSpeechTags(words: Array[String], tags: Array[String]): Array[String] = {
    for(i <- words.indices) {

      // "due" in "due to" must be a preposition
      if(i < words.length - 1 &&
        words(i).equalsIgnoreCase("due") &&
        words(i + 1).equalsIgnoreCase("to")) {
        tags(i) = "IN"
      }

      else if(VERSUS_PATTERN.findFirstIn(words(i)).nonEmpty) {
        tags(i) = "CC" // "versus" seems like a CC to me. but maybe not...
      }
    }

    tags
  }

  /** 
   * Deterministic corrections for dependency parsing 
   * Modifies headsWithLabels in place
   */
  def parserPostProcessing(sentence: Sentence, headsWithLabels: Array[(Int, String)]): Unit = {
    for(i <- sentence.indices) {
      // "due to" must be a MWE
      if(i < sentence.size - 1 && sentence.tags.isDefined &&
        sentence.words(i).compareToIgnoreCase("due") == 0 &&
        sentence.tags.get(i) == "IN" &&
        sentence.words(i + 1).compareToIgnoreCase("to") == 0 &&
        sentence.tags.get(i + 1) == "TO" &&
        headsWithLabels(i + 1)._1 != i &&
        headsWithLabels(i + 1)._2 != "mwe") {
        headsWithLabels(i + 1) = Tuple2(i, "mwe")
      }
    }
  }
}
