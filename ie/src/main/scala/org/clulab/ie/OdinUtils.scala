package org.clulab.ie

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.Mention


/** Utilities for rules and querying taxonomy */
object OdinUtils extends LazyLogging {

  val NODE_LABEL = "Node"
  val CAUSAL_RELATION = "CausalEvent"

  /**
    * Finds the semantic head for a Mention.  If none is found, the rightmost word will be used by default.
    */
  def getSemHeadWord(m: Mention): String = {
    val hwo = m.semHeadWord
    if (hwo.isEmpty) {
      logger.info(s"Error finding '.semHeadWord' for '${m.text}' in sentence: '${m.sentenceObj.getSentenceText}'")
      m.words.last
    } else { hwo.get }
  }
}
