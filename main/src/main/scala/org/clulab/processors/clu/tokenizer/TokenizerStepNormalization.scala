package org.clulab.processors.clu.tokenizer

import scala.collection.mutable.ArrayBuffer
import TokenizerStepNormalization._
import org.clulab.utils.ScienceUtils

class TokenizerStepNormalization extends TokenizerStep {
  // used for text normalization
  lazy val scienceUtils = new ScienceUtils

  def process(inputs:Array[RawToken]):Array[RawToken] = {
    val output = new ArrayBuffer[RawToken]

    for(input <- inputs) {
      // convert all parens in the format that Treebank likes
      if(PARENS.contains(input.word)) {
        output += RawToken(
          input.raw,
          input.beginPosition,
          input.endPosition,
          PARENS(input.word))
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
