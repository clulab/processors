package org.clulab.processors.clu.tokenizer

abstract class Contraction(letters: String, val exceptions: Seq[String]) {
  val letterGroups = Contraction.toLetterGroups(letters)
  val length = letters.length

  def expandWithoutException(rawToken: RawToken): Array[RawToken]

  def expand(rawToken: RawToken): Array[RawToken] =
      // Try not to lowercase unless necessary.
      if (exceptions.nonEmpty && exceptions.contains(rawToken.raw.toLowerCase)) Array(rawToken)
      else expandWithoutException(rawToken)

  def split(rawToken: RawToken, n: Int = length): (String, String) = rawToken.raw.splitAt(rawToken.raw.length - n)

  def matches(rawToken: RawToken): Boolean = {
    val raw = rawToken.raw
    val rawLength = raw.length

    length <= rawLength && letterGroups.indices.forall { index =>
      // This takes care of case and searching from the right.
      letterGroups(length - 1 - index).contains(raw.charAt(rawLength - 1 - index))
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
    val (leftRaw, rightRaw) = split(rawToken)
    val rightToken = RawToken(rightRaw, rawToken.beginPosition + leftRaw.length)

    if (leftRaw.nonEmpty)
      Array(RawToken(leftRaw, rawToken.beginPosition), rightToken)
    else
      Array(rightToken)
  }
}

// Perform a full match and use the left and right words.  This is mostly for exceptions to
// other rules so that they should come first.
class BothContraction(letters: String, contractionLength: Int, leftWord: String, rightWord: String, exceptions: String*) extends Contraction(letters, exceptions) {

  override def expandWithoutException(rawToken: RawToken): Array[RawToken] = {
    val (leftRaw, rightRaw) = split(rawToken, contractionLength)

    Array(
      RawToken(leftRaw, rawToken.beginPosition, leftWord),
      RawToken(rightRaw, rawToken.beginPosition + leftRaw.length, rightWord)
    )
  }

  override def matches(rawToken: RawToken): Boolean =
      // Since it is a full match, the length needs to be exact.
      length == rawToken.raw.length && super.matches(rawToken)
}

// Match again from the right and expand only the right side, using the remainder (if any) for the left.
class RightContraction(letters: String, rightWord: String, exceptions: String*) extends Contraction(letters, exceptions) {

  override def expandWithoutException(rawToken: RawToken): Array[RawToken] = {
    val (leftRaw, rightRaw) = split(rawToken)
    val rightToken = RawToken(rightRaw, rawToken.beginPosition + leftRaw.length, rightWord)

    if (leftRaw.nonEmpty)
      Array(RawToken(leftRaw, rawToken.beginPosition), rightToken)
    else
      Array(rightToken)
  }
}

class TokenizerStepContractions extends TokenizerStep {

  // Unlike CoreNLP, we allow single quotes inside words
  // We must separate important linguistic constructs here
  override def process(rawTokens: Array[RawToken]): Array[RawToken] = {
    rawTokens.flatMap { rawToken =>
      // An apostrophe heralds all contractions, so resort to expensive
      // matching only after the cheap apostrophe detector sounds.
      // Option.when works nicely in Scala 3.
      (if (rawToken.raw.contains('\'')) Some(true) else None)
          .flatMap(_ => TokenizerStepContractions.contractions.find(_.matches(rawToken)))
          .map(_.expand(rawToken))
          .getOrElse(Array(rawToken))
    }
  }
}

object TokenizerStepContractions {
  val contractions = Array(
    new    BothContraction("won't", 3, "will", "not"),
    new    BothContraction("can't", 3, "can", "not"),
    new    BothContraction("shan't", 3, "shall", "not"),
    new NeitherContraction("'s", "let's"),         // person's -> person 's
    new   RightContraction("n't", "not", "ain't"), // don't -> do not
    new   RightContraction("'m", "am"),            // I'm -> I am
    new NeitherContraction("'d", "cont'd"),        // he'd -> he 'd, with one exception
    new   RightContraction("'ll", "will"),         // she'll -> she will
    new   RightContraction("'ve", "have")          // they've -> they have
  )
}
