package org.clulab.processors.clu.tokenizer

import scala.collection.mutable.ArrayBuffer

/**
  * Resolves English contractions
  * Author: mihais
  * Date: 3/21/17
  */
class TokenizerStepContractions extends TokenizerStep {
  private val genitive = """'[sS]$""".r // Regex.pattern is a val stored inside this class, so accessing it is cheap
  private val wont = """^[wW][oO][nN]'[tT]$""".r
  private val nt = """[nN]'[tT]$""".r
  private val appm = """'[mM]$""".r
  private val appd = """'[dD]$""".r
  private val appl = """'[lL][lL]$""".r

  override def process(inputs:Array[RawToken]): Array[RawToken] = {
    //
    // Unlike CoreNLP, we allow single quotes inside words
    // We must separate important linguistic constructs here
    // TODO: this is slow. This should be handled in the Antlr grammar
    //

    val tokens = new ArrayBuffer[RawToken]()

    for(input <- inputs) {
      // An apostrophe heralds all contractions, so resort to expensive
      // regular expressions only after the cheap apostrophe detector sounds.
      if (!input.raw.contains('\''))
        tokens += input
      else {
        // genitive
        if (genitive.findFirstIn(input.raw).isDefined && input.raw.length > 2) {
          tokens += RawToken(input.raw.substring(0, input.raw.length - 2), input.beginPosition)
          // TODO: can we detect if genitive or "is" here?
          tokens += RawToken(input.raw.substring(input.raw.length - 2), input.beginPosition + input.raw.length - 2)
        }

        // "won't"
        else if (wont.findFirstIn(input.raw).isDefined) {
          tokens += RawToken(input.raw.substring(0, 2), input.beginPosition, "will")
          tokens += RawToken(input.raw.substring(2), input.beginPosition + 2, "not")
        }

        // other words ending with "n't"
        else if (nt.findFirstIn(input.raw).isDefined) {
          if (input.raw.length > 3) {
            tokens += RawToken(input.raw.substring(0, input.raw.length - 3), input.beginPosition, input.raw.substring(0, input.raw.length - 3))
            tokens += RawToken(input.raw.substring(input.raw.length - 3), input.beginPosition + input.raw.length - 3, "not")
          } else {
            tokens += RawToken(input.raw, input.beginPosition, input.endPosition, "not")
          }
        }

        // words ending with "'m"
        else if (appm.findFirstIn(input.raw).isDefined) {
          if (input.raw.length > 2) {
            tokens += RawToken(input.raw.substring(0, input.raw.length - 2), input.beginPosition)
            tokens += RawToken(input.raw.substring(input.raw.length - 2), input.beginPosition + input.raw.length - 2, "am")
          } else {
            tokens += RawToken(input.raw, input.beginPosition, "am")
          }
        }

        // words ending with "'d"
        else if (appd.findFirstIn(input.raw).isDefined && input.raw.length > 2 && !(input.raw.toLowerCase == "cont'd")) {
          tokens += RawToken(input.raw.substring(0, input.raw.length - 2), input.beginPosition)
          // TODO: can we detect if "would" or "had" here?
          tokens += RawToken(input.raw.substring(input.raw.length - 2), input.beginPosition + input.raw.length - 2)
        }

        // words ending with "'ll"
        else if (appl.findFirstIn(input.raw).isDefined) {
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
    }

    tokens.toArray
  }
}

