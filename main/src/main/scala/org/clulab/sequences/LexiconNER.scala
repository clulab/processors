package org.clulab.sequences

import java.util.function.Consumer

import org.clulab.processors.Sentence
import org.clulab.struct.CompactLexiconNER
import org.clulab.struct.DebugBooleanHashTrie
import org.clulab.struct.IntHashTrie
import org.clulab.struct.{BooleanHashTrie, EntityValidator, TrueEntityValidator}
import org.clulab.utils.Files.loadStreamFromClasspath
import org.clulab.utils.Serializer
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

abstract class LexiconNER(knownCaseInsensitives: Set[String], useLemmas: Boolean) extends Tagger[String] with Serializable {
  /**
    * Matches the lexicons against this sentence
    * @param sentence The input sentence
    * @return An array of BIO notations the store the outcome of the matches
    */
  def find(sentence: Sentence): Array[String]
  def getLabels: Seq[String]
  def toString(stringBuilder: StringBuilder): Unit // This is to help with debugging.

  protected def contentfulSpan(sentence: Sentence, start: Int, length: Int):Boolean = {
    val (characters, letters, digits, upperCaseLetters, spaces) =
      LexiconNER.scanText(sentence.words, start, start + length)
    // a valid span must have letters > 0 and at least one of the other properties
    val contentful = letters > 0 && (
      digits > 0 ||
      upperCaseLetters > 0 ||
      spaces > 0 ||
      characters > LexiconNER.KNOWN_CASE_INSENSITIVE_LENGTH ||
      knownCaseInsensitives.contains(sentence.words(start))
    )

    contentful
  }

  protected def getTokens(sentence: Sentence): Array[String] =
      if (useLemmas) sentence.lemmas.get
      else sentence.words
}


object LexiconNER {
  // These are configuration values that affect object creation.
  val USE_FAST = true // Otherwise, slow will be used
  val USE_COMPACT = false // This applies to fast only
  val USE_DEBUG = false // This applies to the slow one only
  val OVERRIDE_ENTITY_VALIDATOR = false // true is only for testing!

  val OUTSIDE_LABEL: String = "O"
  val KNOWN_CASE_INSENSITIVE_LENGTH: Int = 3 // this was tuned for Reach; if changed please rerun Reach unit tests

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
  def apply(kbs: Seq[String], overrideKBs: Option[Seq[String]],
      entityValidator: EntityValidator,
      lexicalVariationEngine: LexicalVariations,
      useLemmasForMatching: Boolean,
      caseInsensitiveMatching: Boolean): LexiconNER = {
    val newEntityValidator =
        if (OVERRIDE_ENTITY_VALIDATOR) EntityValidator.TRUE_VALIDATOR
        else  entityValidator

    if (USE_FAST)
      new FastLexiconNERBuilder(caseInsensitiveMatching).build(kbs, overrideKBs, newEntityValidator,
          lexicalVariationEngine, useLemmasForMatching)
    else
      new SlowLexiconNERBuilder(caseInsensitiveMatching).build(kbs, overrideKBs, newEntityValidator,
          lexicalVariationEngine, useLemmasForMatching)
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
    val spaces = words.length - 1

    for (offset <- start until end) {
      val word = words(offset)
      for (i <- word.indices) {
        val c = word.charAt(i)
        characters += 1
        if (Character.isLetter(c)) letters += 1
        if (Character.isDigit(c)) digits += 1
        if (Character.isUpperCase(c)) upperCaseLetters += 1
      }
    }
    (characters, letters, digits, upperCaseLetters, spaces)
  }

  /** Merges labels from src into dst, without overlapping any existing labels in dst */
  def mergeLabels(dst: Array[String], src: Array[String]) {
    assert(dst.length == src.length)

    var offset = 0
    while (offset < dst.length) {
      if (src(offset) != LexiconNER.OUTSIDE_LABEL) {
        // no overlap allowed
        // if overlap is detected, the corresponding labels in src are discarded
        if (!overlap(dst, src, offset)) {
          dst(offset) = src(offset)
          offset += 1
          while (offset < src.length && src(offset).startsWith("I-")) {
            dst(offset) = src(offset)
            offset += 1
          }
        }
        else {
          // overlap found; just ignore the labels in src
          offset += 1
          while (offset < src.length && src(offset).startsWith("I-")) {
            offset += 1
          }
        }
      }
      else {
        // nothing to merge
        offset += 1
      }
    }
  }

  // Used by mergeLabels above
  private def overlap(dst: Array[String], src: Array[String], offset: Int): Boolean = {
    var position = offset

    if (dst(position) != LexiconNER.OUTSIDE_LABEL)
      true
    else {
      position += 1
      while (position < src.length && src(position).startsWith("I-")) {
        if (dst(position) != LexiconNER.OUTSIDE_LABEL)
          return true
        position += 1
      }
      false
    }
  }
}

abstract class LexiconNERBuilder(caseInsensitiveMatching: Boolean) {
  val logger: Logger = LoggerFactory.getLogger(classOf[LexiconNER])
  val INTERN_STRINGS: Boolean = false

  def build(kbs: Seq[String], overrideKBs: Option[Seq[String]],
    entityValidator: EntityValidator, lexicalVariationEngine: LexicalVariations, useLemmasForMatching: Boolean): LexiconNER

  protected def toJavaConsumer[T](consumer: T => Unit): Consumer[T] = {
    new Consumer[T] {
      override def accept(t: T): Unit = consumer(t)
    }
  }

  protected def extractKBName(kb: String): String = {
    val slash = kb.lastIndexOf("/")
    val dot = kb.indexOf('.')
    val name = kb.substring(slash + 1, dot)
    name
  }
}

class SlowLexiconNERBuilder(caseInsensitiveMatching: Boolean) extends LexiconNERBuilder(caseInsensitiveMatching) {

  class BuildState(lexicalVariationEngine: LexicalVariations) {
    val knownCaseInsensitives = new mutable.HashSet[String]()

    def addWithLexicalVariations(matcher: BooleanHashTrie, tokens: Array[String]): Unit = {
      // keep track of all lower case ents that are single token (necessary for case-insensitive matching)
      if (tokens.length == 1) {
        val token = tokens(0)
        if (token.toLowerCase == token && caseInsensitiveMatching)
          knownCaseInsensitives.add(tokens(0))
      }
      // add the original form
      matcher.add(tokens)
      // add the lexical variations
      for (ts <- lexicalVariationEngine.lexicalVariations(tokens)) {
        matcher.add(ts)
      }
    }
  }

  protected def newMatcher(label: String): BooleanHashTrie =
    if (LexiconNER.USE_DEBUG)
      new DebugBooleanHashTrie(label, caseInsensitive = caseInsensitiveMatching, internStrings = INTERN_STRINGS)
    else
      new BooleanHashTrie(label, caseInsensitive = caseInsensitiveMatching, internStrings = INTERN_STRINGS)

  override def build(kbs: Seq[String], overrideKBs: Option[Seq[String]],
      entityValidator: EntityValidator, lexicalVariationEngine: LexicalVariations, useLemmasForMatching: Boolean): SeparatedLexiconNER = {
    logger.info("Beginning to load the KBs for the rule-based bio NER...")
    val buildState = new BuildState(lexicalVariationEngine)
    val overrideMatchers = getOverrideMatchers(overrideKBs, buildState) // first so they take priority during matching
    val standardMatchers = getStandardMatchers(kbs, buildState)
    logger.info("KB loading completed.")
    new SeparatedLexiconNER((overrideMatchers ++ standardMatchers).toArray, buildState.knownCaseInsensitives.toSet,
        useLemmasForMatching, entityValidator)
  }

  private def getStandardMatchers(kbs: Seq[String], buildState: BuildState): Seq[BooleanHashTrie] = {
    def addLine(matcher: BooleanHashTrie, inputLine: String): Unit = {
      val line = inputLine.trim
      if (!line.startsWith("#")) {
        val tokens = line.split("\\s+")
        buildState.addWithLexicalVariations(matcher, tokens)
      }
    }

    kbs.map { kb =>
      val label = extractKBName(kb)
      val matcher = newMatcher(label)

      Serializer.using(loadStreamFromClasspath(kb)) { reader =>
        reader.lines.forEach(toJavaConsumer[String] { line: String =>
          addLine(matcher, line)
        })
      }
      logger.info(s"Loaded matcher for label $label. The size of the first layer is ${matcher.entriesSize}.")
      matcher
    }
  }

  private def getOverrideMatchers(overrideKBs: Option[Seq[String]], buildState: BuildState): Seq[BooleanHashTrie] = {
    // in these KBs, the name of the entity must be the first token, and the label must be the last
    //   (tokenization is performed around TABs here)
    def addLine(matchers: mutable.HashMap[String, BooleanHashTrie], inputLine: String): Unit = {
      val line = inputLine.trim
      if (!line.startsWith("#")) { // skip comments starting with #
        val blocks = line.split("\t")
        if (blocks.size >= 2) {
          val entity = blocks.head // grab the text of the named entity
          val label = blocks.last // grab the label of the named entity
          val tokens = entity.split("\\s+")
          val matcher = matchers.getOrElseUpdate(label, new BooleanHashTrie(label))

          buildState.addWithLexicalVariations(matcher, tokens)
        }
      }
    }

    if (overrideKBs.isDefined) {
      val matchers = new mutable.HashMap[String, BooleanHashTrie]()

      overrideKBs.get.foreach { okb =>
        Serializer.using(loadStreamFromClasspath(okb)) { reader =>
          reader.lines.forEach(toJavaConsumer[String] { line =>
            addLine(matchers, line)
          })
        }
      }
      for (name <- matchers.keySet.toList.sorted) {
        val matcher = matchers(name)
        logger.info(s"Loaded OVERRIDE matcher for label $name.  The size of the first layer is ${matcher.entriesSize}.")
      }
      matchers.values.toSeq
    }
    else Seq.empty[BooleanHashTrie]
  }
}

class FastLexiconNERBuilder(caseInsensitiveMatching: Boolean) extends LexiconNERBuilder(caseInsensitiveMatching) {

  class BuildState(lexicalVariationEngine: LexicalVariations) {
    val intHashTrie = new IntHashTrie
    val labelToIndex = new mutable.HashMap[String, Int] // This just maps the labels to an integer
    val knownCaseInsensitives = new mutable.HashSet[String]()

    def getCount: Int = intHashTrie.entriesSize

    protected def add(label: String, tokens: Array[String]): Unit = {
      val index = labelToIndex.getOrElseUpdate(label, labelToIndex.size)

      intHashTrie.add(tokens, index)
    }

    def addWithLexicalVariations(label: String, tokens: Array[String]): Unit = {
      // keep track of all lower case ents that are single token (necessary for case-insensitive matching)
      if (tokens.length == 1) {
        val token = tokens(0)
        if (token.toLowerCase == token && caseInsensitiveMatching)
          knownCaseInsensitives.add(tokens(0))
      }
      // add the original form
      add(label, tokens)
      // add the lexical variations
      for (ts <- lexicalVariationEngine.lexicalVariations(tokens)) {
        add(label, ts)
      }
    }
  }

  protected def newMatcher(label: String) =
      new IntHashTrie(caseInsensitive = caseInsensitiveMatching, internStrings = INTERN_STRINGS)

  override def build(kbs: Seq[String], overrideKBs: Option[Seq[String]],
      entityValidator: EntityValidator, lexicalVariationEngine: LexicalVariations, useLemmasForMatching: Boolean): LexiconNER = {
    logger.info("Beginning to load the KBs for the rule-based bio NER...")
    val buildState = new BuildState(lexicalVariationEngine)
    addOverrideKBs(overrideKBs, buildState) // first so they take priority during matching
    addStandardKBs(kbs, buildState)
    logger.info("KB loading completed.")

    val labels = buildState.labelToIndex.toArray.sortBy(_._2).map(_._1)

    if (LexiconNER.USE_COMPACT)
      CompactLexiconNER(buildState.intHashTrie, labels, buildState.knownCaseInsensitives.toSet, useLemmasForMatching, entityValidator)
    else
      new CombinedLexiconNER(buildState.intHashTrie, labels, buildState.knownCaseInsensitives.toSet, useLemmasForMatching, entityValidator)
  }

  private def addStandardKBs(kbs: Seq[String], buildState: BuildState): Unit = {
    def addLine(inputLine: String, label: String): Unit = {
      val line = inputLine.trim
      if (!line.startsWith("#")) {
        val tokens = line.split("\\s+")
        buildState.addWithLexicalVariations(label, tokens)
      }
    }

    kbs.foreach { kb =>
      val label = extractKBName(kb)
      val beforeCount = buildState.getCount
      Serializer.using(loadStreamFromClasspath(kb)) { reader =>
        reader.lines.forEach(toJavaConsumer[String] { line: String =>
          addLine(line, label)
        })
      }
      var afterCount = buildState.getCount
      logger.info(s"Loaded matcher for label $label. The number of entries added to the first layer was ${afterCount - beforeCount}.")
    }
  }

  private def addOverrideKBs(overrideKBs: Option[Seq[String]], buildState: BuildState): Unit = {
    // in these KBs, the name of the entity must be the first token, and the label must be the last
    //   (tokenization is performed around TABs here)
    def addLine(inputLine: String): Unit = {
      val line = inputLine.trim
      if (!line.startsWith("#")) { // skip comments starting with #
        val blocks = line.split("\t")
        if (blocks.size >= 2) {
          val entity = blocks.head   // grab the text of the named entity
          val label = blocks.last    // grab the label of the named entity
          val tokens = entity.split("\\s+")
          buildState.addWithLexicalVariations(label, tokens)
        }
      }
    }

    overrideKBs.getOrElse(Seq.empty).foreach { okb =>
      val beforeCount = buildState.getCount
      Serializer.using(loadStreamFromClasspath(okb)) { reader =>
        reader.lines.forEach(toJavaConsumer[String] { line: String =>
          addLine(line)
        })
      }
      var afterCount = buildState.getCount
      logger.info(s"Loaded OVERRIDE matchers for all labels.  The number of entries added to the first layer was ${afterCount - beforeCount}.")
    }
  }
}
