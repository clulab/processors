package org.clulab.processors.clu.tokenizer

import scala.collection.mutable.ArrayBuffer

/**
 * Tokenizes some hyphenated prefixes, which are better handled downstream as separate tokens
 *   For example: "mid-July" is separated into "mid" and "July", which is better for date recognition
 */
class TokenizerStepHyphens extends TokenizerStep {
  private val prefixes = """^(mid|bi|semi|non|all)\-\w+$""".r // TODO: what other prefixes should we add?

  override def process(inputs: Array[RawToken]): Array[RawToken] = {
    val tokens = new ArrayBuffer[RawToken]()

    for(input <- inputs) {
      // A dash heralds all the prefixes we handle here, so resort to expensive
      // regular expressions only after the cheap dash detector sounds.
      if (!input.raw.contains('-'))
        tokens += input
      else {
        if (prefixes.findFirstIn(input.raw).isDefined) {
          val dashPosition = input.raw.indexOf('-')
          tokens += RawToken(input.raw.substring(0, dashPosition), 0, dashPosition)
          tokens += RawToken(input.raw.substring(dashPosition + 1), dashPosition + 1)
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
