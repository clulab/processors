package org.clulab.processors.clu.sequences

import org.clulab.processors.Sentence
import org.clulab.processors.clu.SentencePostProcessor

/**
  * Fixes some common POS tagging mistakes for PT (in place)
  * User: mihais
  * Date: 10/10/17
  */
class PortuguesePOSPostProcessor extends SentencePostProcessor {
  override def process(sentence: Sentence): Unit = {
    val tags = sentence.tags.get
    val words = sentence.words

    for(i <- sentence.indices) {
      val word = words(i)
      val tag = tags(i)

      // TODO: add postproc operations here
    }
  }
}

