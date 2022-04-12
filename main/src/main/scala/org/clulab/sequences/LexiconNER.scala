package org.clulab.sequences

import org.clulab.processors.Sentence
import org.clulab.struct.{EntityValidator, TrueEntityValidator}

import java.io.File
import scala.collection.mutable

/**
  * The abstract base class for several concrete child classes used for Named Entity
  * Recognition (NER) based on the contents of lexica, which are lists of words and
  * phrases representing named entities
  *
  * For all of these classes, NER labels are derived from the file names of the lexica
  * or the records in overrideKBs by the [[org.clulab.sequences.LexiconNERBuilder LexiconNERBuilders]].
  * This class, via variables [[org.clulab.sequences.LexiconNER.USE_FAST USE_FAST]] and
  * [[org.clulab.sequences.LexiconNER.USE_COMPACT USE_COMPACT]], controls which builder
  * use used.
  *
  * The collection of child classes is small:
  * <ul>
  *   <li>
  *     The [[org.clulab.sequences.SeparatedLexiconNER SeparatedLexiconNER]] is closest
  *     to the original implementation.  It has a [[org.clulab.struct.BooleanHashTrie BooleanHashTrie]]
  *     for each label and in that trie, Boolean values indicate that the sequence of
  *     strings leading there is a named entity.  Each trie structure must be searched for
  *     potential named entities.
  *   </li>
  *   <li>
  *     The [[org.clulab.sequences.CombinedLexiconNER CombinedLexiconNER]] stores instead
  *     of the Boolean in the BooleanHashTrie an Int in an [[org.clulab.struct.IntHashTrie IntHashTrie]].
  *     The Int indicates which of the labels is the one to use for the entity just found.
  *     In this way, only one trie (or two if there are different case sensitivity settings)
  *     needs to be searched no matter how many labels there are (at least until Integer.MAX_VALUE).
  *   </li>
  *   <li>
  *     The [[org.clulab.sequences.CompactLexiconNER CompactLexiconNER]] uses the same
  *     strategy to minimize the number of tries, but also converts the tries into
  *     [[org.clulab.sequences.CompactTrie CompactTries]] which consist of arrays
  *     of integers indicating offsets into other arrays.  In this way the time it takes
  *     to de/serialize the NER is reduced, and some lookup operations are made more efficient.
  *   </li>
  * </ul>
  *
  * @param knownCaseInsensitives Words known to appear with and without capitalized letters which help determine
  * whether a span of text is contentful
  * @param useLemmas If false, use the words of a sentence; if true, the lemmas
  */
// These knownCaseInsensitives are used to check for contentful spans.
abstract class LexiconNER(val knownCaseInsensitives: Set[String], val useLemmas: Boolean) extends Tagger[String] with Serializable {
  /**
    * Matches the lexicons against this sentence
    * @param sentence The input sentence
    * @return An array of BIO notations the store the outcome of the matches
    */
  def find(sentence: Sentence): Array[String]
  def getLabels: Seq[String]

  /**
    * The class is serializable and this method is used during testing to determine whether a reconstitued
    * object is equal to the original without interfering with the operation of equals and getting into
    * hash codes.  Is is not necessary for this operation to be efficient or complete.
    * @param other The object to compare to
    * @return Whether this and other are equal, at least as far is serialization is concerned
    */
  def equalsForSerialization(other: AnyRef): Boolean = {
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
  /**
    * When true indicates use of the [[org.clulab.sequences.FastLexiconNERBuilder FastLexiconNERBuilder]]
    * and otherwise the [[org.clulab.sequences.SlowLexiconNERBuilder SlowLexiconNERBuilder]]
    * to construct the LexiconNER
    */
  var USE_FAST = true
  /**
    * If the [[org.clulab.sequences.FastLexiconNERBuilder FastLexiconNERBuilder]] is beind used,
    * indicates when true that a [[org.clulab.sequences.CompactLexiconNER CompactLexiconNER]]
    * should be created and otherwise a [[org.clulab.sequences.CombinedLexiconNER CombinedLexiconNER]]
    */
  var USE_COMPACT = true
  val USE_DEBUG = false // This applies to the slow one only
  val OVERRIDE_ENTITY_VALIDATOR = false // true is only for testing!

  val OUTSIDE_LABEL: String = "O"
  val KNOWN_CASE_INSENSITIVE_LENGTH: Int = 3 // this was tuned for Reach; if changed please rerun Reach unit tests

  /** Create a LexiconNER from a pair of sequences of knowledge base sources for the kbs and overrideKBs.
    * There are versions of the sources for knowledge bases stored in files and those stored in memory.
    * Each StandardKbSource knows its own caseSensitivityMatching, so no list of those need be supplied.
    * It is possible that contents of an overrideKB refers to a KB (label) that does not exist.  In that
    * situation, caseInsensitiveMatching is used as a fallback value.  With that, this method should
    * encompass all the functionality of the other apply methods, which now feed into it.
    * Note that some of the arugments are in a different order than the other build methods in order
    * to overload the method despite type erasure.
    *
    * @param standardKbSources KB sources containing known entity names
    * @param overrideKbSourcesOpt KB sources containing override labels for entity names from kbs (necessary for the bio domain)
    * @param entityValidator Filter which decides if a matched entity is valid
    * @param caseInsensitiveMatchings case insensitivities corresponding to the kbs, matched by index
    * @param lexicalVariationEngine Generates alternative spellings of an entity name (necessary for the bio domain)
    * @param useLemmasForMatching If true, we use Sentence.lemmas instead of Sentence.words during matching
    * @param defaultCaseInsensitive If true, tokens are matched case insensitively
    * @return The new LexiconNER
    */
  def apply(
    standardKbSources: Seq[StandardKbSource], overrideKbSourcesOpt: Option[Seq[OverrideKbSource]],
    // The change in order here is to allow a method overload despite type erasure.
    lexicalVariationEngine: LexicalVariations, entityValidator: EntityValidator, useLemmasForMatching: Boolean,
    // This default value is used when there are overrideKBs with labels that don't match a regular KB.
    defaultCaseInsensitive: Boolean
  ): LexiconNER = {
    val newEntityValidator =
      if (OVERRIDE_ENTITY_VALIDATOR) EntityValidator.TRUE_VALIDATOR
      else  entityValidator
    val builder =
      if (USE_FAST) new FastLexiconNERBuilder(USE_COMPACT)
      else new SlowLexiconNERBuilder()

    builder.build(standardKbSources, overrideKbSourcesOpt, newEntityValidator, lexicalVariationEngine,
      useLemmasForMatching, defaultCaseInsensitive)
  }

  /** Create a LexiconNER from a pair of sequences of knowledge bases (KBs), the kbs and overrideKBs,
    * with control over the case sensitivity of individual KBs via caseInsensitiveMatchings
    *
    * The matchings run parallel to the KBs.  That is, caseInsensitiveMatchings(n) is used for kbs(n).
    * It is possible that contents of an overrideKB refers to a KB that does not exist.  In that
    * situation, caseInsensitiveMatching is used as a fallback value.
    *
    * @param kbs KBs containing known entity names
    * @param overrideKBs KBs containing override labels for entity names from kbs (necessary for the bio domain)
    * @param caseInsensitiveMatchings case insensitivities corresponding to the kbs, matched by index
    * @param entityValidator Filter which decides if a matched entity is valid
    * @param lexicalVariationEngine Generates alternative spellings of an entity name (necessary for the bio domain)
    * @param useLemmasForMatching If true, we use Sentence.lemmas instead of Sentence.words during matching
    * @param defaultCaseInsensitive If true, tokens are matched case insensitively
    * @param baseDirOpt An optional directory to force kbs to be loaded from files rather than resources
    * @return The new LexiconNER
    */
  def apply(kbs: Seq[String], overrideKBs: Option[Seq[String]], caseInsensitiveMatchings: Seq[Boolean],
      entityValidator: EntityValidator, lexicalVariationEngine: LexicalVariations, useLemmasForMatching: Boolean,
      defaultCaseInsensitive: Boolean, baseDirOpt: Option[File]
  ): LexiconNER = this(
    kbs.zip(caseInsensitiveMatchings).map { case (kb,caseInsensitiveMatchings) =>
      if (baseDirOpt.isEmpty)
        new ResourceStandardKbSource(kb, caseInsensitiveMatchings)
      else
        new FileStandardKbSource(kb, caseInsensitiveMatchings, baseDirOpt.get)
    },
    overrideKBs.map { overrideKBs =>
      overrideKBs.map { overrideKB =>
        if (baseDirOpt.isEmpty)
          new ResourceOverrideKbSource(overrideKB)
        else
          new FileOverrideKbSource(overrideKB, baseDirOpt.get)
      }
    },
    lexicalVariationEngine, entityValidator, useLemmasForMatching, defaultCaseInsensitive
  )

  /** Same apply with some default values filled in */
  def apply(kbs: Seq[String],
            caseInsensitiveMatchings: Seq[Boolean],
            entityValidator: EntityValidator,
            useLemmasForMatching: Boolean): LexiconNER = {
    apply(
      kbs, None, caseInsensitiveMatchings,
      entityValidator, new NoLexicalVariations, useLemmasForMatching,
      defaultCaseInsensitive = false,
      baseDirOpt = None
    )
  }

  /** Same apply with more default values filled in */
  def apply(kbs: Seq[String],
            caseInsensitiveMatchings: Seq[Boolean],
            useLemmasForMatching: Boolean): LexiconNER = {
    apply(
      kbs, None, caseInsensitiveMatchings,
      new TrueEntityValidator, new NoLexicalVariations, useLemmasForMatching,
      defaultCaseInsensitive = false,
      baseDirOpt = None
    )
  }

  /** Same apply with even more default values filled in */
  def apply(kbs: Seq[String], caseInsensitiveMatchings: Seq[Boolean]): LexiconNER = {
    apply(
      kbs, None, caseInsensitiveMatchings,
      new TrueEntityValidator, new NoLexicalVariations,
      useLemmasForMatching = false,
      defaultCaseInsensitive = false,
      baseDirOpt = None
    )
  }

  /** This is just like the above but with the addition of the baseDirOpt. */
  def apply(kbs: Seq[String], caseInsensitiveMatchings: Seq[Boolean], baseDirOpt: Option[File]): LexiconNER = {
    apply(
      kbs, None, caseInsensitiveMatchings,
      new TrueEntityValidator, new NoLexicalVariations,
      useLemmasForMatching = false,
      defaultCaseInsensitive = false,
      baseDirOpt = baseDirOpt
    )
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
        useLemmasForMatching, caseInsensitiveMatching, None)
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
  def mergeLabels(dst: Array[String], src: Array[String]): Unit = {
    assert(dst.length == src.length)
    // Pick one or take the average of both.
    val length = dst.length
    // Skip over all the outside labels so that we will start notOutside.
    var offset = countWhile(src, 0, isOutside)

    while (offset < length) {
      val notOutsideCount = countWhile(src, offset, isNotOutside)
      // Check that there is not anything in dst that should not be overwritten.
      if (!dst.view(offset, offset + notOutsideCount).exists(isNotOutside))
        Array.copy(src, offset, dst, offset, notOutsideCount)
      offset += notOutsideCount

      val outsideCount = countWhile(src, offset, isOutside)
      offset += outsideCount
    }
  }
}
