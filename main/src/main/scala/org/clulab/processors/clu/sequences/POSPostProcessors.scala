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
      val lower = word.toLowerCase()

      word match {
        case adj if lower == "normovolêmica" => tags(i) = "ADJ"
        case adj if lower == "autóloga" => tags(i) = "ADJ"
        case adj if lower == "lática" => tags(i) = "ADJ"
        case adj if lower == "protéica" => tags(i) = "ADJ"

        case adj if lower == "cardiovascular" => tags(i) = "ADJ"
        case adj if lower == "vascular" => tags(i) = "ADJ"
        case adj if lower == "foliar" => tags(i) = "ADJ"

        case noun if lower == "câncer" => tags(i) = "NOUN"
        case noun if lower == "aminoácido" => tags(i) = "NOUN"
        case noun if lower == "aminoácidos" => tags(i) = "NOUN"

        case noun if lower == "béquer" => tags(i) = "NOUN"
        case noun if lower == "açúcar" => tags(i) = "NOUN"
        case noun if lower == "amplificador" => tags(i) = "NOUN"

        case "-LRB-" => tags(i) = "PUNCT"
        case "-RRB-" => tags(i) = "PUNCT"

        case _ => ()
      }
    }
  }
}
