package org.clulab.processors.clu.tokenizer

import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer

class TokenizerStepContractions extends TokenizerStep {

  abstract class Contraction(pattern: Pattern, length: Int) {

    def split(rawToken: RawToken, tokens: ArrayBuffer[RawToken]): Unit

    def splitRight(string: String, n: Int): (String, String) = {
      val split = string.length - n

      (string.substring(0, split), string.substring(split))
    }

    def matches(rawToken: RawToken): Boolean =
        // Check the length before the expensive match.
        length <= rawToken.raw.length && pattern.matcher(rawToken.raw).find

    def expand(rawToken: RawToken, tokens: ArrayBuffer[RawToken]): Unit =
        split(rawToken, tokens)
  }

  // Match from the right but expand neither left nor right side while still separating them.
  class NeitherContraction(pattern: Pattern, length: Int) extends Contraction(pattern, length) {

    override def split(rawToken: RawToken, tokens: ArrayBuffer[RawToken]): Unit = {
      val (leftRaw, rightRaw) = splitRight(rawToken.raw, length)

      if (leftRaw.nonEmpty)
        tokens += RawToken(leftRaw, rawToken.beginPosition)
      tokens += RawToken(rightRaw, rawToken.beginPosition + leftRaw.length)
    }
  }

  // Perform a full match and use the left and right words.  This is mostly for exceptions to
  // other rules so that they should come first.
  class BothContraction(pattern: Pattern, length: Int, leftWord: String, rightWord: String) extends Contraction(pattern, length) {

    override def split(rawToken: RawToken, tokens: ArrayBuffer[RawToken]): Unit = {
      val (leftRaw, rightRaw) = splitRight(rawToken.raw, length)

      tokens += RawToken(leftRaw, rawToken.beginPosition, leftWord)
      tokens += RawToken(rightRaw, rawToken.beginPosition + leftRaw.length, rightWord)
    }
  }

  // Match again from the right and expand only the right side, using the remainder (if any) for the left.
  class RightContraction(pattern: Pattern, length: Int, rightWord: String) extends Contraction(pattern, length) {

    override def split(rawToken: RawToken, tokens: ArrayBuffer[RawToken]): Unit = {
      val (leftRaw, rightRaw) = splitRight(rawToken.raw, length)

      if (leftRaw.nonEmpty)
        tokens += RawToken(leftRaw, rawToken.beginPosition)
      tokens += RawToken(rightRaw, rawToken.beginPosition + leftRaw.length, rightWord)
    }
  }

  def toPattern(template: String): Pattern = template.r.pattern

  val contractions = Array(
    new    BothContraction(toPattern("^[wW][oO][nN]'[tT]$"), 5, "will", "not"), // won't -> will not
    new NeitherContraction(toPattern("'[sS]$"),              2),                // person's -> person 's
    new   RightContraction(toPattern("[nN]'[tT]$"),          3, "not"),         // don't -> do not
    new   RightContraction(toPattern("'[mM]$"),              2, "am"),          // I'm -> I am
    new NeitherContraction(toPattern("'[dD]$"),              2),                // he'd -> he 'd
    new   RightContraction(toPattern("'[lL][lL]$"),          3, "will")         // she'll -> she will
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
