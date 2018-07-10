package org.clulab.processors.clu.tokenizer

/**
  * Stores a token as produced by a tokenizer
  * @param raw The EXACT text tokenized
  * @param beginPosition beginning character offset of raw
  * @param endPosition end character offset of raw
  * @param word Normalized form raw, e.g., "'m" becomes "am". Note: these are NOT lemmas.
  */
case class RawToken(raw:String, beginPosition:Int, endPosition:Int, word:String) {
  override def toString: String = s"[$raw, $beginPosition, $endPosition, $word]"
}

object RawToken {
  def apply(raw:String, beginPosition:Int, word:String): RawToken = {
    RawToken(raw, beginPosition, beginPosition + raw.length, word)
  }

  def apply(raw:String, beginPosition:Int): RawToken = {
    RawToken(raw, beginPosition, beginPosition + raw.length, raw)
  }

  def apply(raw:String, beginPosition:Int, endPosition:Int): RawToken = {
    RawToken(raw, beginPosition, endPosition, raw)
  }
}
