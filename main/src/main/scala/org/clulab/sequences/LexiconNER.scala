package org.clulab.sequences

import org.clulab.processors.Sentence
import org.clulab.struct.{EntityValidator, TrueEntityValidator}

import scala.collection.mutable

// These knownCaseInsensitives are used to check for contentful spans.
abstract class LexiconNER(val knownCaseInsensitives: Set[String], val useLemmas: Boolean) extends Tagger[String] with Serializable {
  /**
    * Matches the lexicons against this sentence
    * @param sentence The input sentence
    * @return An array of BIO notations the store the outcome of the matches
    */
  def find(sentence: Sentence): Array[String]
  def getLabels: Seq[String]


  def isCloseEnough(other: AnyRef): Boolean = {
    other.isInstanceOf[LexiconNER] && {
      val that = other.asInstanceOf[LexiconNER]

      this.useLemmas == that.useLemmas &&
      this.knownCaseInsensitives == that.knownCaseInsensitives
    }
  }

  def hasCondition(wordsView: mutable.IndexedSeqView[String, Array[String]], condition: Char => Boolean): Boolean =
    wordsView.exists(_.exists(condition))

  def hasLetter(wordsView: mutable.IndexedSeqView[String, Array[String]]): Boolean =
    hasCondition(wordsView, Character.isLetter)

  def hasDigit(wordsView: mutable.IndexedSeqView[String, Array[String]]): Boolean =
    hasCondition(wordsView, Character.isDigit)

  def hasUpperCaseLetters(wordsView: mutable.IndexedSeqView[String, Array[String]]): Boolean =
    hasCondition(wordsView, Character.isUpperCase)

  def hasSpace(wordsView: mutable.IndexedSeqView[String, Array[String]]): Boolean = wordsView.length > 1

  def countCharacters(wordsView: mutable.IndexedSeqView[String, Array[String]]): Int =
    // Go ahead and calculate them all even though we only need to know if they exceed a value.
    wordsView.foldLeft(0) { (sum, word) => sum + word.length }

  val contentQualifiers: Array[mutable.IndexedSeqView[String, Array[String]] => Boolean] = Array(
    // Start with the quick and easy ones.
    hasSpace,
    { wordsView => countCharacters(wordsView) > LexiconNER.KNOWN_CASE_INSENSITIVE_LENGTH },
    hasDigit,
    hasUpperCaseLetters,
    { wordsView => knownCaseInsensitives.contains(wordsView.head) }
  )

  protected def contentfulSpan(sentence: Sentence, start: Int, length: Int): Boolean = {
    val wordsView = sentence.words.view(start, start + length)
    // A valid view/span must have a letter and at least one of the other qualifiers.
    val contentful = hasLetter(wordsView) && contentQualifiers.exists(_(wordsView))

    contentful
  }

  protected val getTokens: Sentence => Array[String] =
    // Decide this once and for all and don't revisit it each time getTokens is called.
    if (useLemmas) getLemmas
    else getWords

  protected def getLemmas(sentence: Sentence): Array[String] = sentence.lemmas.get

  protected def getWords(sentence: Sentence): Array[String] = sentence.words
}

object LexiconNER {
  // These are configuration values that affect object creation.
  var USE_FAST = true // Otherwise, slow will be used
  var USE_COMPACT = true // This applies to fast only
  val USE_DEBUG = false // This applies to the slow one only
  val OVERRIDE_ENTITY_VALIDATOR = false // true is only for testing!

  val OUTSIDE_LABEL: String = "O"
  val KNOWN_CASE_INSENSITIVE_LENGTH: Int = 3 // this was tuned for Reach; if changed please rerun Reach unit tests

  def apply(kbs: Seq[String], overrideKBs: Option[Seq[String]], caseInsensitiveMatchings: Seq[Boolean],
      entityValidator: EntityValidator, lexicalVariationEngine: LexicalVariations, useLemmasForMatching: Boolean,
      defaultCaseInsensitive: Boolean): LexiconNER = {
    val newEntityValidator =
      if (OVERRIDE_ENTITY_VALIDATOR) EntityValidator.TRUE_VALIDATOR
      else  entityValidator
    val builder =
      if (USE_FAST) new FastLexiconNERBuilder()
      else new SlowLexiconNERBuilder()

    builder.build(kbs, caseInsensitiveMatchings, overrideKBs, newEntityValidator, lexicalVariationEngine,
        useLemmasForMatching, defaultCaseInsensitive)
  }

  /**
    * Creates a LexiconNER from a list of KBs
    * Note that file name (minus the extension) for each KB becomes the name of the corresponding category.
    *   For example, /Some/Path/SomeCategory.tsv.gz yields the category name SomeCategory.
    * Each of the KBs must contain one entity name per line
    *
    * @param kbs KBs containing known entity names
    * @param overrideKBs KBs containing override labels for entity names from kbs (necessary for the bio domain)
    * @param entityValidator Filter which decides if a matched entity is valid
    * @param lexicalVariationEngine Generates alternative spellings of an entity name (necessary for the bio domain)
    * @param useLemmasForMatching If true, we use Sentence.lemmas instead of Sentence.words during matching
    * @param caseInsensitiveMatching If true, tokens are matched case insensitively
    * @return The new LexiconNER
    */

  // Expand the caseInsensitiveMatching to plural and fill in parallel structure
  // Do not initialize builder with the overall value.
  // Pass the thing in the build arguments.
  def apply(kbs: Seq[String], overrideKBs: Option[Seq[String]],
      entityValidator: EntityValidator,
      lexicalVariationEngine: LexicalVariations,
      useLemmasForMatching: Boolean,
      caseInsensitiveMatching: Boolean): LexiconNER = {
    val caseInsensitiveMatchings = Array.fill(kbs.length)(caseInsensitiveMatching)
    this(kbs, overrideKBs, caseInsensitiveMatchings, entityValidator, lexicalVariationEngine,
        useLemmasForMatching, caseInsensitiveMatching)
  }

  /**
    * Creates a LexiconNER from a list of KBs
    * Note that file name (minus the extension) for each KB becomes the name of the corresponding category.
    *   For example, /Some/Path/SomeCategory.tsv.gz yields the category name SomeCategory.
    * Each of the KBs must contain one entity name per line
    *
    * @param kbs KBs containing known entity names
    * @param entityValidator Filter which decides if a matched entity is valid
    * @param useLemmasForMatching If true, we use Sentence.lemmas instead of Sentence.words during matching
    * @param caseInsensitiveMatching If true, tokens are matched case insensitively
    * @return The new LexiconNER
    */
  def apply(kbs: Seq[String],
      entityValidator: EntityValidator = new TrueEntityValidator,
      useLemmasForMatching: Boolean = false,
      caseInsensitiveMatching: Boolean = false): LexiconNER = {
    apply(kbs, None, entityValidator, new NoLexicalVariations, useLemmasForMatching, caseInsensitiveMatching)
  }

  def scanText(words: Array[String], start: Int, end: Int): (Int, Int, Int, Int, Int) = {
    var characters = 0
    var letters = 0
    var digits = 0
    var upperCaseLetters = 0
    val spaces = math.max(0, end - start - 1) // Spaces are between words, not after them.

    words.view(start, end).foreach { word =>
      characters += word.length
      word.foreach { c =>
        if (Character.isLetter(c)) letters += 1
        if (Character.isDigit(c)) digits += 1
        if (Character.isUpperCase(c)) upperCaseLetters += 1
      }
    }
    (characters, letters, digits, upperCaseLetters, spaces)
  }

  def isOutside(label: String): Boolean = label == OUTSIDE_LABEL
  def isNotOutside(label: String): Boolean = !isOutside(label)

  def countWhile(labels: Array[String], offset: Int, condition: String => Boolean): Int = {
    var count = 0

    while (offset + count < labels.length && condition(labels(offset + count)))
      count += 1
    count
  }

  /** Merges labels from src into dst without overlapping any existing labels in dst. */
  def mergeLabels(dst: Array[String], src: Array[String]) {
    assert(dst.length == src.length)
    // Pick one or take the average of both.
    val length = dst.length
    // Skip over all the outside labels so that we will start notOutside.
    var offset = countWhile(src, 0, isOutside)

    while (offset < length) {
      val notOutsideCount = countWhile(src, offset, isNotOutside)
      // Check that there is not anything in dst that should not be overwritten.
      if (!dst.view(offset, offset + notOutsideCount).exists(isNotOutside))
        src.copyToArray(dst, offset, notOutsideCount)
      offset += notOutsideCount

      val outsideCount = countWhile(src, offset, isOutside)
      offset += outsideCount
    }
  }
}
