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

/**
 * For this kind of contraction, neither the left nor right side is expanded even though the two
 * halves will be separated (unless the word is an exception).  For example, "we'd" with "we" on
 * the left and "'d" on the right, is converted only to "we 'd" because the "'d" is ambiguous and
 * could mean either "would" or "had".
 *
 * @constructor create a new contraction
 * @param letters the contraction, e.g., "'d" or "n't"
 * @param exceptions entire words which match the letters but shouldn't be expanded, e.g., "cont'd"
 */
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

/**
 * For this kind of contraction, the left and right sides are expanded to pre-determined words, usually
 * because the left side is an exception and the right side is a contraction.  For example, "can't" gets
 * expanded to "can" rather than "ca" and "n't" becomes "not", resulting in "can not".  All the letters
 * must match, so there is no room for exceptions.  This is mostly for special cases of other contraction
 * conventions so that this kind should come first.
 *
 * @constructor create a new contraction
 * @param letters all the letters needed for a match including the contraction, e.g., "can't"
 * @param contractionLength the number of letters in the contraction, e.g., 3 for "n't"
 * @param leftWord the left side after expansion, e.g, "can"
 * @param rightWord the right side after expansion, e.g., "not"
 */
class BothContraction(letters: String, contractionLength: Int, leftWord: String, rightWord: String) extends Contraction(letters, Seq.empty) {

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

/**
 * For this kind of contraction, only the right side is expanded.  The left side can be deduced from the
 * contracted word.  For example "we've" expands to "we", already part of the word, and "have", which is
 * supplied as an argument.  "We've" becomes thus "We have".  Exceptions can be provided for cases in
 * which there is a "'ve" but the left side should not be reused verbatim.
 *
 * @constructor create a new contraction
 * @param letters the contraction, e.g., "'ve"
 * @param rightWord the right side after expansion, e.g., "have"
 * @param exceptions entire words which match the letters but shouldn't be expanded
 */
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
    new NeitherContraction("'s", "let's"),         // person's -> person 's, with one exception
    new NeitherContraction("'d", "cont'd"),        // he'd -> he 'd, with one exception
    new   RightContraction("n't", "not", "ain't"), // don't -> do not
    new   RightContraction("'m", "am"),            // I'm -> I am
    new   RightContraction("'ll", "will"),         // she'll -> she will
    new   RightContraction("'ve", "have"),         // we've -> we have
    new   RightContraction("'re", "are")           // they're -> they are
  )
}
