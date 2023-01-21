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

    def expand(rawToken: RawToken, tokens: ArrayBuffer[RawToken]): Unit = {
      val raw = rawToken.raw

      if (length == raw.length && raw != "won't")
        tokens += RawToken(raw, rawToken.beginPosition, rightOpt.getOrElse(raw))
      else
        split(rawToken, tokens)
      true
    }
  }

  // Both has both sides, matches from start to end of word
  // Neither has no sides, matches from right
  // Right, matches from right
  // add exceptions, matches but don't apply

  def toPattern(template: String): Pattern = template.r.pattern

  val contractions = Array(
    new Contraction(toPattern("^[wW][oO][nN]'[tT]$"), 5, Some("will"), Some("not")), // won't -> will not
    new Contraction(toPattern("'[sS]$"),              2, None,         None),        // person's -> person 's
    new Contraction(toPattern("[nN]'[tT]$"),          3, None,         Some("not")), // don't -> do not
    new Contraction(toPattern("'[mM]$"),              2, None,         Some("am")),  // I'm -> I am
    new Contraction(toPattern("'[dD]$"),              2, None,         None),        // he'd -> he 'd
    new Contraction(toPattern("'[lL][lL]$"),          3, None,         Some("will")) // she'll -> she will
  )

  // Unlike CoreNLP, we allow single quotes inside words
  // We must separate important linguistic constructs here
  override def process(inputs: Array[RawToken]): Array[RawToken] = {
    val output = new ArrayBuffer[RawToken]()

    inputs.foreach { input =>
      // An apostrophe heralds all contractions, so resort to expensive
      // regular expressions only after the cheap apostrophe detector sounds.
      val heralded = input.raw.contains('\'')
      val contractionOpt =
          if (heralded) contractions.lift(contractions.indexWhere(_.matches(input)))
          else None

      if (contractionOpt.isDefined)
        contractionOpt.foreach(_.expand(input, output))
      else
        output += input
    }
    output.toArray
  }
}
