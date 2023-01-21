package org.clulab.processors.clu.tokenizer

import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer

class TokenizerStepContractions extends TokenizerStep {
  // Regex.pattern is a val stored inside this class, so accessing it is cheap.
  def toPattern(string: String): Pattern = string.r.pattern

  // TODO: Pre-calculate length of string needed to match it.
  private val WON_T = toPattern("""^[wW][oO][nN]'[tT]$""") // won't -> will not
  private val    _S = toPattern("""'[sS]$""")              // person's -> he is
  private val   N_T = toPattern("""[nN]'[tT]$""")          // don't -> do not
  private val    _M = toPattern("""'[mM]$""")              // I'm -> I am
  private val    _D = toPattern("""'[dD]$""")              // he'd -> he would or he had
  private val   _LL = toPattern("""'[lL][lL]$""")          // he'll -> he will

  override def process(inputs: Array[RawToken]): Array[RawToken] = {
    val output = new ArrayBuffer[RawToken]()

    inputs.foreach { input =>
      // An apostrophe heralds all contractions, so resort to expensive
      // regular expressions only after the cheap apostrophe detector sounds.
      if (!input.raw.contains('\''))
        output += input
      else process(input, output)
    }
    output.toArray
  }

  // Unlike CoreNLP, we allow single quotes inside words
  // We must separate important linguistic constructs here
  protected def process(input: RawToken, tokens: ArrayBuffer[RawToken]): Unit = {
    val raw = input.raw
    val rawLength = raw.length

    def splitRight(n: Int, leftWordOpt: Option[String] = None, rightWordOpt: Option[String] = None): Unit = {
      val leftRaw = raw.substring(0, rawLength - n)
      val rightRaw = raw.substring(rawLength - n)

      val leftWord = leftWordOpt.getOrElse(leftRaw)
      val rightWord = rightWordOpt.getOrElse(rightRaw)

      if (leftRaw.nonEmpty)
        tokens += RawToken(leftRaw, input.beginPosition, leftWord)
      tokens += RawToken(rightRaw, input.beginPosition + rawLength - n, rightWord)
    }

    // genitive
    if (_S.matcher(raw).find) {
      // TODO: can we detect if genitive or "is" here?
      if (rawLength > 2) splitRight(2)
      else tokens += input
    }
    else if (WON_T.matcher(raw).find) {
      splitRight(2, Some("will"), Some("not"))
    }
    else if (N_T.matcher(raw).find) {
      if (rawLength > 3) splitRight(3, None, Some("not"))
      else tokens += RawToken(raw, input.beginPosition, "not") // n't -> not
    }
    else if (_M.matcher(raw).find) {
      if (rawLength > 2) splitRight(2, None, Some("am"))
      else tokens += RawToken(raw, input.beginPosition, "am") // 'm -> am
    }
    else if (_D.matcher(raw).find) {
      // TODO: can we detect if "would" or "had" here?
      if (rawLength > 2 && raw.toLowerCase != "cont'd") splitRight(2)
      else tokens += input // 'd -> 'd
    }
    else if (_LL.matcher(raw).find) {
      if (rawLength > 3) splitRight(3, None, Some("will"))
      else tokens += RawToken(raw, input.beginPosition, "will") // 'll -> will
    }
    else tokens += input
  }
}
