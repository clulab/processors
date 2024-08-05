package org.clulab.sequences

import org.clulab.processors.Sentence

/**
  * High-level trait for a sequence tagger
  * User: mihais
  * Date: 10/12/17
  */
trait Tagger[L] {
  def find(sentence:Sentence):Array[L]
}
