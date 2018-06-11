package org.clulab.processors.clu.tokenizer

import scala.collection.mutable.ArrayBuffer

/**
  * Resolves English contractions
  * Author: mihais
  * Date: 3/21/17
  */
class TokenizerStepContractions extends TokenizerStep {
  override def process(inputs:Array[RawToken]): Array[RawToken] = {
    //
    // Unlike CoreNLP, we allow single quotes inside words
    // We must separate important linguistic constructs here
    // TODO: this is slow. This should be handled in the Antlr grammar
    //

    val tokens = new ArrayBuffer[RawToken]()

    for(input <- inputs) {

      // genitive
      if ("""'[sS]$""".r.findFirstIn(input.raw).isDefined && input.raw.length > 2) {
        tokens += RawToken(input.raw.substring(0, input.raw.length - 2), input.beginPosition)
        // TODO: can we detect if genitive or "is" here?
        tokens += RawToken(input.raw.substring(input.raw.length - 2), input.beginPosition + input.raw.length - 2)
      }

      // "won't"
      else if ("""^[wW][oO][nN]'[tT]$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 2), input.beginPosition, "will")
        tokens += RawToken(input.raw.substring(2), input.beginPosition + 2, "not")
      }

      // other words ending with "n't"
      else if ("""[nN]'[tT]$""".r.findFirstIn(input.raw).isDefined) {
        if (input.raw.length > 3) {
          tokens += RawToken(input.raw.substring(0, input.raw.length - 3), input.beginPosition, input.raw.substring(0, input.raw.length - 3))
          tokens += RawToken(input.raw.substring(input.raw.length - 3), input.beginPosition + input.raw.length - 3, "not")
        } else {
          tokens += RawToken(input.raw, input.beginPosition, input.endPosition, "not")
        }
      }

      // words ending with "'m"
      else if ("""'[mM]$""".r.findFirstIn(input.raw).isDefined) {
        if (input.raw.length > 2) {
          tokens += RawToken(input.raw.substring(0, input.raw.length - 2), input.beginPosition)
          tokens += RawToken(input.raw.substring(input.raw.length - 2), input.beginPosition + input.raw.length - 2, "am")
        } else {
          tokens += RawToken(input.raw, input.beginPosition, "am")
        }
      }

      // words ending with "'d"
      else if ("""'[dD]$""".r.findFirstIn(input.raw).isDefined && input.raw.length > 2 && !(input.raw.toLowerCase == "cont'd")) {
        tokens += RawToken(input.raw.substring(0, input.raw.length - 2), input.beginPosition)
        // TODO: can we detect if "would" or "had" here?
        tokens += RawToken(input.raw.substring(input.raw.length - 2), input.beginPosition + input.raw.length - 2)
      }

      // words ending with "'ll"
      else if ("""'[lL][lL]$""".r.findFirstIn(input.raw).isDefined) {
        if (input.raw.length > 3) {
          tokens += RawToken(input.raw.substring(0, input.raw.length - 3), input.beginPosition)
          tokens += RawToken(input.raw.substring(input.raw.length - 3), input.beginPosition + input.raw.length - 3, "will")
        } else {
          tokens += RawToken("will", input.beginPosition, input.endPosition)
        }
      }

      // any other token
      else {
        tokens += input
      }
    }

    tokens.toArray
  }
}

