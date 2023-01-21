package org.clulab.processors.clu.tokenizer

import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer

class TokenizerStepContractions extends TokenizerStep {

  // Full won't
  // Conditional cont'd

  class Contraction(pattern: Pattern, length: Int, leftOpt: Option[String], rightOpt: Option[String]) {

    def matches(rawToken: RawToken): Boolean =
        // Check the length before the expensive match.
        length <= rawToken.raw.length && pattern.matcher(rawToken.raw).find

    def split(rawToken: RawToken, tokens: ArrayBuffer[RawToken]): Unit = {
      val raw = rawToken.raw
      val rawLength = raw.length

      val leftRaw = raw.substring(0, rawLength - length)
      val rightRaw = raw.substring(rawLength - length)

      val leftWord = leftOpt.getOrElse(leftRaw)
      val rightWord = rightOpt.getOrElse(rightRaw)

//      if (leftRaw.nonEmpty)
        tokens += RawToken(leftRaw, rawToken.beginPosition, leftWord)
      tokens += RawToken(rightRaw, rawToken.beginPosition + rawLength - length, rightWord)
    }

    def expand(rawToken: RawToken, tokens: ArrayBuffer[RawToken]): Boolean = {
      val raw = rawToken.raw

      if (length == raw.length && raw != "won't")
        tokens += RawToken(raw, rawToken.beginPosition, rightOpt.getOrElse(raw))
      else
        split(rawToken, tokens)
      true
    }
  }

  object Contraction {

    def apply(template: String, length: Int, leftOpt: Option[String], rightOpt: Option[String]): Contraction =
      new Contraction(template.r.pattern, length, leftOpt, rightOpt)
  }

  // TODO: Pre-calculate length of string needed to match it.
  private val WON_T = Contraction("""^[wW][oO][nN]'[tT]$""", 5, Some("will"), Some("not"))  // won't -> will not
  private val    _S = Contraction("""'[sS]$""",              2, None,         None)         // person's -> he is
  private val   N_T = Contraction("""[nN]'[tT]$""",          3, None,         Some("not"))  // don't -> do not
  private val    _M = Contraction("""'[mM]$""",              2, None,         Some("am"))   // I'm -> I am
  private val    _D = Contraction("""'[dD]$""",              2, None,         None)         // he'd -> he would or he had
  private val   _LL = Contraction("""'[lL][lL]$""",          3, None,         Some("will")) // he'll -> he will

  override def process(inputs: Array[RawToken]): Array[RawToken] = {
    val output = new ArrayBuffer[RawToken]()
    // TODO: Add the output to function so that don't have to pass it?

    inputs.foreach { input =>
      // An apostrophe heralds all contractions, so resort to expensive
      // regular expressions only after the cheap apostrophe detector sounds.
      if (!input.raw.contains('\'') || !process(input, output))
        output += input
    }
    output.toArray
  }

  // Unlike CoreNLP, we allow single quotes inside words
  // We must separate important linguistic constructs here
  protected def process(input: RawToken, tokens: ArrayBuffer[RawToken]): Boolean = {
    if (false) true
    else if (_S.matches(input)) _S.expand(input, tokens)
    else if (WON_T.matches(input)) WON_T.expand(input, tokens)
    else if (N_T.matches(input)) N_T.expand(input, tokens)
    else if (_M.matches(input)) _M.expand(input, tokens)
    else if (_D.matches(input)) {
      if (input.raw.toLowerCase != "cont'd") _D.expand(input, tokens)
      else false
    }
    else if (_LL.matches(input)) _LL.expand(input, tokens)
    else false
  }
}
