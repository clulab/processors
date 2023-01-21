package org.clulab.processors.clu.tokenizer

abstract class Contraction(letters: String, exceptions: Seq[String]) {
  val letterGroups = Contraction.toLetterGroups(letters)
  val length = letters.length

  def expandWithoutException(rawToken: RawToken): Array[RawToken]

  def isException(string: String): Boolean = {
    val lowerRaw = string.toLowerCase

    exceptions.exists(_ == lowerRaw)
  }

  def expand(rawToken: RawToken): Array[RawToken] = {
    // Try not to lowercase unless necessary.
    if (exceptions.nonEmpty && isException(rawToken.raw)) Array(rawToken)
    else expandWithoutException(rawToken)
  }

  def splitRight(string: String, n: Int): (String, String) = {
    val split = string.length - n

    (string.substring(0, split), string.substring(split))
  }

  def matches(rawToken: RawToken): Boolean = {
    val raw = rawToken.raw
    val rawLength = raw.length

    length <= rawLength && letterGroups.indices.forall { index =>
      // This takes care of case.
      letterGroups(length - 1 - index).indexOf(raw.charAt(rawLength - 1 - index)) >= 0
    }
  }
}

object Contraction {
  type LetterGroups = Array[String]

  def toLetterGroups(template: String): LetterGroups = template.toArray.map { letter =>
    if (letter.toLower == letter.toUpper) letter.toString
    else letter.toLower.toString + letter.toUpper
  }
}

// Match from the right but expand neither left nor right side while still separating them.
class NeitherContraction(letters: String, exceptions: String*) extends Contraction(letters, exceptions) {

  override def expandWithoutException(rawToken: RawToken): Array[RawToken] = {
    val (leftRaw, rightRaw) = splitRight(rawToken.raw, length)
    val rightToken = RawToken(rightRaw, rawToken.beginPosition + leftRaw.length)

    if (leftRaw.nonEmpty)
      Array(RawToken(leftRaw, rawToken.beginPosition), rightToken)
    else
      Array(rightToken)
  }
}

// Perform a full match and use the left and right words.  This is mostly for exceptions to
// other rules so that they should come first.
class BothContraction(letters: String, leftWord: String, rightWord: String, exceptions: String*) extends Contraction(letters, exceptions) {

  override def expandWithoutException(rawToken: RawToken): Array[RawToken] = {
    val (leftRaw, rightRaw) = splitRight(rawToken.raw, length)

    Array(
      RawToken(leftRaw, rawToken.beginPosition, leftWord),
      RawToken(rightRaw, rawToken.beginPosition + leftRaw.length, rightWord)
    )
  }

  override def matches(rawToken: RawToken): Boolean = {
    // Since it is a full match, the length needs to be exact.
    length == rawToken.raw.length && super.matches(rawToken)
  }
}

// Match again from the right and expand only the right side, using the remainder (if any) for the left.
class RightContraction(letters: String, rightWord: String, exceptions: String*) extends Contraction(letters, exceptions) {

  override def expandWithoutException(rawToken: RawToken): Array[RawToken] = {
    val (leftRaw, rightRaw) = splitRight(rawToken.raw, length)
    val rightToken = RawToken(rightRaw, rawToken.beginPosition + leftRaw.length, rightWord)

    if (leftRaw.nonEmpty)
      Array(RawToken(leftRaw, rawToken.beginPosition), rightToken)
    else
      Array(rightToken)
  }
}

class TokenizerStepContractions extends TokenizerStep {
  import TokenizerStepContractions.contractions

  // Unlike CoreNLP, we allow single quotes inside words
  // We must separate important linguistic constructs here
  override def process(inputs: Array[RawToken]): Array[RawToken] = {
    inputs.flatMap { input =>
      // An apostrophe heralds all contractions, so resort to expensive
      // matching only after the cheap apostrophe detector sounds.
      val heralded = input.raw.contains('\'')
      val contractionOpt =
          if (heralded) contractions.lift(contractions.indexWhere(_.matches(input)))
          else None

      contractionOpt
          .map(_.expand(input))
          .getOrElse(Array(input))
    }
  }
}

object TokenizerStepContractions {
  val contractions = Array(
    new    BothContraction("won't", "will", "not"), // won't -> will not
    new NeitherContraction("'s"),                   // person's -> person 's
    new   RightContraction("n't", "not"),           // don't -> do not
    new   RightContraction("'m", "am"),             // I'm -> I am
    new NeitherContraction("'d", "cont'd"),         // he'd -> he 'd, with one exception
    new   RightContraction("'ll", "will")           // she'll -> she will
  )
}
