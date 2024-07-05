package org.clulab.processors.clu.tokenizer

/**
  * Implements one step of a tokenization algorithm, which takes in a sequence of tokens and produces another
  * For example, contractions such as "don't" are handled here; domain-specific operations as well.
  * Note: one constraint that must be obeyed by any TokenizerStep is that
  *       RawToken.raw and the corresponding character positions must preserve the original text
  */
trait TokenizerStep {
  def process(inputs:Array[RawToken]):Array[RawToken]
}

