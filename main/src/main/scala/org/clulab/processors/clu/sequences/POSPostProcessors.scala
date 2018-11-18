package org.clulab.processors.clu.sequences

import org.clulab.processors.Sentence
import org.clulab.processors.clu.SentencePostProcessor

import scala.util.matching.Regex

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

/**
  * Fixes some common POS tagging mistakes for EN (in place)
  * User: mihais
  * Date: 11/18/18
  */
class EnglishPOSPostProcessor extends SentencePostProcessor {
  override def process(sentence: Sentence): Unit = {
    val tags = sentence.tags.get
    val words = sentence.words

    for(i <- sentence.indices) {
      val word = words(i)
      val tag = tags(i)

      //
      // Make sure parens are tagged correctly
      // TODO: better fix: change POS tags for parens in the corpus used to train the CLU POS tagger!
      //
      if(LEFT_PARENS.findFirstMatchIn(word).nonEmpty) {
        tags(i) = "-LRB-"
      } else if(RIGHT_PARENS.findFirstMatchIn(word).nonEmpty) {
        tags(i) = "-RRB-"
      }
    }
  }

  val LEFT_PARENS: Regex = """^(\-LRB\-)|(\-LSB\-)|(-LCB-)|\(|\[|\{$""".r
  val RIGHT_PARENS: Regex = """^(\-RRB\-)|(\-RSB\-)|(-RCB-)|\)|\]|\}$""".r
}

