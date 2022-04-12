package org.clulab.processors.clu.tokenizer

import scala.collection.mutable.ArrayBuffer
import TokenizerStepNormalization._
import org.clulab.utils.ScienceUtils

class TokenizerStepNormalization extends TokenizerStep {
  // used for text normalization
  lazy val scienceUtils = new ScienceUtils

  //
  // the corpora used to train the dependency parsers and the NERs keep parentheses NOT normalized
  // the original Treebank used to train the constituent parser normalized them
  // so, we'll keep parens NOT normalized since this is more common, and normalize them inside the constituent parser
  //     (see CoreNLPProcessor)
  //
  val normalizeParens = false

  def process(inputs:Array[RawToken]):Array[RawToken] = {
    val output = new ArrayBuffer[RawToken]

    // count quotes; even numbers indicate opening quote; odd numbers indicate closing ones
    // we make the simplifying assumption here that there are no nested quotes
    var quoteCount = 0
    val quoteCounts = new Array[Int](inputs.length)
    for(i <- inputs.indices) {
      if(inputs(i).raw == "\"") {
        quoteCounts(i) = quoteCount
        quoteCount += 1
      } else {
        quoteCounts(i) = -1
      }
    }

    for(i <- inputs.indices) {
      val input = inputs(i)

      // convert all parens in the format that Treebank likes
      if(normalizeParens && PARENS.contains(input.word)) {
        output += RawToken(
          input.raw,
          input.beginPosition,
          input.endPosition,
          PARENS(input.word))
      }

      // replace double-quote characters with Treebank quotes
      else if(input.raw == "\"") {
        val word = if(quoteCounts(i) % 2 == 0) "``" else "''"
        output += RawToken(
          input.raw,
          input.beginPosition,
          input.endPosition,
          word
        )
      }

      // some tokens may contain white spaces, e.g., SGML blocks, and malt barfs on these
      else if("""\s""".r.findFirstIn(input.word).isDefined) {
        output += RawToken(
          input.raw,
          input.beginPosition,
          input.endPosition,
          input.word.replaceAll("\\s", "_"))
      }

      // replace common Unicode characters with the corresponding ASCII string, e.g., \u0277 is replaced with "omega"
      else {
        output += RawToken(
          input.raw,
          input.beginPosition,
          input.endPosition,
          scienceUtils.replaceUnicodeWithAscii(input.word)
        )
      }
    }

    output.toArray
  }
}

/**
  * Normalize text while keeping crucial accented characters, e.g. 'á'.
  */
class TokenizerStepAccentedNormalization extends TokenizerStepNormalization {
  override def process(inputs:Array[RawToken]):Array[RawToken] = {
    val output = new ArrayBuffer[RawToken]

    // count quotes; even numbers indicate opening quote; odd numbers indicate closing ones
    // we make the simplifying assumption here that there are no nested quotes
    var quoteCount = 0
    val quoteCounts = new Array[Int](inputs.length)
    for(i <- inputs.indices) {
      if(inputs(i).raw == "\"") {
        quoteCounts(i) = quoteCount
        quoteCount += 1
      } else {
        quoteCounts(i) = -1
      }
    }

    for(i <- inputs.indices) {
      val input = inputs(i)

      // convert all parens in the format that Treebank likes
      if(PARENS.contains(input.word)) {
        output += RawToken(
          input.raw,
          input.beginPosition,
          input.endPosition,
          PARENS(input.word))
      }

      // replace double-quote characters with Treebank quotes
      else if(input.raw == "\"") {
        val word = if(quoteCounts(i) % 2 == 0) "``" else "''"
        output += RawToken(
          input.raw,
          input.beginPosition,
          input.endPosition,
          word
        )
      }

      // some tokens may contain white spaces, e.g., SGML blocks, and malt barfs on these
      else if("""\s""".r.findFirstIn(input.word).isDefined) {
        output += RawToken(
          input.raw,
          input.beginPosition,
          input.endPosition,
          input.word.replaceAll("\\s", "_"))
      }

      // replace common Unicode characters with the corresponding ASCII string, e.g., \u0277 is replaced with "omega"
      else {
        output += RawToken(
          input.raw,
          input.beginPosition,
          input.endPosition,
          scienceUtils.replaceUnicodeWithAscii(input.word, keepAccents = true)
        )
      }
    }

    output.toArray
  }
}

object TokenizerStepNormalization {
  val PARENS = Map(
    "(" -> "-LRB-",
    ")" -> "-RRB-",
    "[" -> "-LSB-",
    "]" -> "-RSB-",
    "{" -> "-LCB-",
    "}" -> "-RCB-"
  )
}
