package org.clulab.processors.clu.tokenizer

import scala.collection.mutable.ListBuffer

import EnglishNormalizer._

/**
  * Normalizes individual tokens
  * Author: mihais
  * Date: 3/21/17
  */
trait Normalizer {
  def normalizeToken(raw:RawToken): Seq[RawToken]
}

class EnglishNormalizer extends Normalizer {
  def normalizeToken(raw:RawToken): Seq[RawToken] = {
    //
    // Unlike CoreNLP, we allow single quotes inside words
    // We must separate important linguistic constructs here
    // TODO: this is slow. This should be handled in the Antlr grammar
    //
    // genitive
    if("""'[sS]$""".r.findFirstIn(raw.text).isDefined && raw.text.length > 2) {
      val tokens = new ListBuffer[RawToken]
      tokens += RawToken(raw.text.substring(0, raw.text.length - 2), raw.startOffset, raw.endOffset - 2)
      // TODO: can we detect if genitive or "is" here?
      tokens += RawToken(raw.text.substring(raw.text.length - 2), raw.startOffset + raw.text.length - 2, raw.endOffset)
      return tokens
    }

    // "won't"
    if("""^[wW][oO][nN]'[tT]$""".r.findFirstIn(raw.text).isDefined) {
      val tokens = new ListBuffer[RawToken]
      tokens += RawToken("will", raw.startOffset, 2)
      tokens += RawToken("not", raw.startOffset + 2, raw.endOffset)
      return tokens
    }

    // other words ending with "n't"
    if("""[nN]'[tT]$""".r.findFirstIn(raw.text).isDefined) {
      val tokens = new ListBuffer[RawToken]
      if(raw.text.length > 3) {
        tokens += RawToken(raw.text.substring(0, raw.text.length - 3), raw.startOffset, raw.endOffset - 3)
        tokens += RawToken("not", raw.startOffset + raw.text.length - 3, raw.endOffset)
      } else {
        tokens += RawToken("not", raw.startOffset, raw.endOffset)
      }
      return tokens
    }

    // words ending with "'m"
    if("""'[mM]$""".r.findFirstIn(raw.text).isDefined) {
      val tokens = new ListBuffer[RawToken]
      if(raw.text.length > 2) {
        tokens += RawToken(raw.text.substring(0, raw.text.length - 2), raw.startOffset, raw.endOffset - 2)
        tokens += RawToken("am", raw.startOffset + raw.text.length - 2, raw.endOffset)
      } else {
        tokens += RawToken("am", raw.startOffset, raw.endOffset)
      }
      return tokens
    }

    // words ending with "'d"
    if("""'[dD]$""".r.findFirstIn(raw.text).isDefined && raw.text.length > 2 && ! (raw.text.toLowerCase == "cont'd")) {
      val tokens = new ListBuffer[RawToken]
      tokens += RawToken(raw.text.substring(0, raw.text.length - 2), raw.startOffset, raw.endOffset - 2)
      // TODO: can we detect if "would" or "had" here?
      tokens += RawToken("'d", raw.startOffset + raw.text.length - 2, raw.endOffset)
      return tokens
    }

    // words ending with "'ll"
    if("""'[lL][lL]$""".r.findFirstIn(raw.text).isDefined) {
      val tokens = new ListBuffer[RawToken]
      if(raw.text.length > 3) {
        tokens += RawToken(raw.text.substring(0, raw.text.length - 3), raw.startOffset, raw.endOffset - 3)
        tokens += RawToken("will", raw.startOffset + raw.text.length - 3, raw.endOffset)
      } else {
        tokens += RawToken("will", raw.startOffset, raw.endOffset)
      }
      return tokens
    }

    // convert all parens in the format that Treebank likes
    if(PARENS.contains(raw.text)) {
      val tokens = new ListBuffer[RawToken]
      tokens += RawToken(PARENS(raw.text), raw.startOffset, raw.endOffset)
      return tokens
    }

    // some tokens may contain white spaces, e.g., SGML blocks
    if("""\s""".r.findFirstIn(raw.text).isDefined) {
      val token = RawToken(raw.text.replaceAll("\\s", "_"), raw.startOffset, raw.endOffset)
      return List(token)
    }

    List(raw)
  }
}

object EnglishNormalizer {
  val PARENS = Map(
    "(" -> "-LRB-",
    ")" -> "-RRB-",
    "[" -> "-LSB-",
    "]" -> "-RSB-",
    "{" -> "-LCB-",
    "}" -> "-RCB-"
  )
}
