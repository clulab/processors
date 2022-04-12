/**
  * This file was extracted from LexiconNER that used to contain both the NERs and the NER builders.
  * Each of these separate builders might eventually be added to the files of their particular NER.
  */

package org.clulab.sequences

import java.util.function.Consumer
import org.clulab.struct.BooleanHashTrie
import org.clulab.struct.DebugBooleanHashTrie
import org.clulab.struct.EntityValidator
import org.clulab.struct.IntHashTrie
import org.clulab.utils.FileUtils
import org.clulab.utils.Files
import org.clulab.utils.Serializer
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import java.io.File
import scala.collection.mutable.{HashMap => MutableHashMap, HashSet => MutableHashSet, Map => MutableMap, Set => MutableSet}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Concrete subclasses are responsible for building various NERs.  The mapping is as follows:
  * <ul>
  *   <li>The [[org.clulab.sequences.SlowLexiconNERBuilder SlowLexiconNERBuilder]] builds a [[org.clulab.sequences.SeparatedLexiconNER SeparatedLexiconNER]].</li>
  *   <li>The [[org.clulab.sequences.FastLexiconNERBuilder FastLexiconNERBuilder]] builds either a
  *     <ul>
  *       <li>[[org.clulab.sequences.CombinedLexiconNER CombinedLexiconNER]] or a</li>
  *       <li>[[org.clulab.sequences.CompactLexiconNER CompactLexiconNER]], depending on the value of useCompact.</li>
  *     </ul>
  *   </li>
  * </ul>
  * For an explanation of how the NERs differ from each other, see their superclass, [[org.clulab.sequences.LexiconNER LexiconNER]].
  */
abstract class LexiconNERBuilder() {
  val logger: Logger = LoggerFactory.getLogger(classOf[LexiconNER])
  val INTERN_STRINGS: Boolean = false

  def build(standardKbSources: Seq[StandardKbSource], overrideKbSourcesOpt: Option[Seq[OverrideKbSource]],
    entityValidator: EntityValidator, lexicalVariationEngine: LexicalVariations, useLemmasForMatching: Boolean,
    // This default value is used when there are overrideKBs with labels that don't match a regular KB.
    defaultCaseInsensitive: Boolean): LexiconNER

  protected def requireUniqueStandardKbSources(standardKbSources: Seq[StandardKbSource]): Unit = {
    val labels = standardKbSources.map(_.getLabel)
    require(labels.distinct.length == labels.length, "KB labels are repeated!")
  }

  // Convert the mutable set into an immutable one consistently.
  protected def toSet(mutableSet: MutableSet[String]): Set[String] = {
    // In the end it was a different set causing problems.
    // val strings = mutableSet.toArray.sorted
    // Set(strings: _*)
    mutableSet.toSet
  }
}

// Heads up.  The following code is tricky because there are many cases to deal with:
// separated, combined, or compact LexiconNERs, standard or override KBs, resource or memory
// storage, and case sensitive or not.
trait KbSource {
  /** removes comments that start with // from the end of the line */
  protected def removeCommentsAndTrim(line: String): String = {
    val commentStart = line.indexOf("//")
    if(commentStart >= 0) {
      line.substring(0, commentStart).trim
    } else {
      line.trim
    }
  }
}

abstract class StandardKbSource(caseInsensitiveMatching: Boolean) extends KbSource {
  def getLabel: String
  def withTokens(f: Array[String] => Unit): Unit

  def getLabel(resourceName: String): String = {
    val slash = resourceName.lastIndexOf("/")
    val dot = resourceName.indexOf('.')
    val label = resourceName.substring(slash + 1, dot)
    label
  }

  def getCaseInsensitiveMatching: Boolean = caseInsensitiveMatching

  protected def processLine(line: String, f: Array[String] => Unit): Unit = {
    val trimmedLine = removeCommentsAndTrim(line)
    if (trimmedLine.nonEmpty && !trimmedLine.startsWith("#")) {
      val tokens = trimmedLine.split("\\s+") // this means entries in the KBs must be pre-tokenized!!
      f(tokens)
    }
  }
}

trait ResourceKbSource {

  def mkConsumer(f: String => Unit): Consumer[String] = {
    new Consumer[String]() {
      def accept(line: String): Unit = {
        f(line)
      }
    }
  }

  def consume(resourceName: String, consumer: Consumer[String]): Unit = {
    Serializer.using(Files.loadStreamFromClasspath(resourceName)) { bufferedReader =>
      bufferedReader.lines.forEach(consumer)
    }
  }

  // This is for testing and only for short files.
  def getLines(resourceName: String): Iterable[String] = {
    var lines: List[String] = Nil

    consume(resourceName, mkConsumer { line: String => lines = line :: lines } )
    lines.reverse
  }
}

class ResourceStandardKbSource(resourceName: String, caseInsensitiveMatching: Boolean)
    extends StandardKbSource(caseInsensitiveMatching) with ResourceKbSource {

  def getLabel: String = getLabel(resourceName)

  def withTokens(f: Array[String] => Unit): Unit = {
    consume(resourceName, mkConsumer { line: String => processLine(line, f) })
  }

  def getLines: Iterable[String] = getLines(resourceName)
}

trait FileKbSource {

  def mkConsumer(f: String => Unit): Consumer[String] = {
    new Consumer[String]() {
      def accept(line: String): Unit = {
        f(line)
      }
    }
  }

  def consume(resourceName: String, baseDir: File, consumer: Consumer[String]): Unit = {
    val file = new File(baseDir, if (resourceName.startsWith("/")) resourceName.drop(1) else resourceName)

    Serializer.using(Files.loadFile(file)) { bufferedReader =>
      bufferedReader.lines.forEach(consumer)
    }
  }

  // This is for testing and only for short files.
  def getLines(resourceName: String, baseDir: File): Iterable[String] = {
    var lines: List[String] = Nil

    consume(resourceName, baseDir, mkConsumer { line: String => lines = line :: lines } )
    lines.reverse
  }
}

class FileStandardKbSource(resourceName: String, caseInsensitiveMatching: Boolean, baseDir: File)
    extends StandardKbSource(caseInsensitiveMatching) with FileKbSource {

  def getLabel: String = getLabel(resourceName)

  def withTokens(f: Array[String] => Unit): Unit = {
    consume(resourceName, baseDir, mkConsumer { line: String => processLine(line, f) })
  }

  def getLines: Iterable[String] = getLines(resourceName, baseDir)
}

class MemoryStandardKbSource(label: String, lines: Iterable[String], caseInsensitiveMatching: Boolean) extends
    StandardKbSource(caseInsensitiveMatching) {

  def getLabel: String = label

  def withTokens(f: Array[String] => Unit): Unit = {
    lines.foreach { line =>
      processLine(line, f)
    }
  }
}

abstract class OverrideKbSource() extends KbSource {
  def withLabelAndTokens(f: (String, Array[String]) => Unit): Unit

  protected def processLine(line: String, f: (String, Array[String]) => Unit): Unit = {
    // In override KBs, the name of the entity must be the first token, and the label must be the last.
    // Tokenization is performed around TABs here.
    val trimmedLine = removeCommentsAndTrim(line)
    if (trimmedLine.nonEmpty && !trimmedLine.startsWith("#")) {
      val blocks = trimmedLine.split("\t")
      if (blocks.size >= 2) {
        val entity = blocks.head // grab the text of the named entity
        val label = blocks.last // grab the label of the named entity
        val tokens = entity.split("\\s+")

        f(label, tokens)
      }
    }
  }
}

class ResourceOverrideKbSource(resourceName: String) extends OverrideKbSource() with ResourceKbSource {

  def withLabelAndTokens(f: (String, Array[String]) => Unit): Unit = {
    consume(resourceName, mkConsumer { line: String => processLine(line, f) })
  }

  def getLines: Iterable[String] = getLines(resourceName)
}

class FileOverrideKbSource(resourceName: String, baseDir: File) extends OverrideKbSource() with FileKbSource {

  def withLabelAndTokens(f: (String, Array[String]) => Unit): Unit = {
    consume(resourceName, baseDir, mkConsumer { line: String => processLine(line, f) })
  }

  def getLines: Iterable[String] = getLines(resourceName, baseDir)
}

class MemoryOverrideKbSource(lines: Iterable[String]) extends OverrideKbSource() {

  override def withLabelAndTokens(f: (String, Array[String]) => Unit): Unit = {
    lines.foreach { line =>
      processLine(line, f)
    }
  }
}

// The BuildState is intended to encapsulate many of the differences between the Slow and Fast
// LexiconNERBuilders and smooth out handling of standard and override KBs as well as the case
// sensitivity options.
trait BuildState

class SlowBuildState(lexicalVariationEngine: LexicalVariations) extends BuildState {
  val knownCaseInsensitives = new MutableHashSet[String]()

  def addWithLexicalVariations(matcher: BooleanHashTrie, tokens: Array[String]): Unit = {
    // Keep track of all lower case entities that are single tokens (necessary for case-insensitive matching).
    if (tokens.length == 1) {
      val token = tokens.head
      val caseInsensitive = matcher.caseInsensitive
      // All single tokens for a case insensitive matcher that are lowercase are added.
      // This collection provides evidence of the contentfulness of the tokens.
      if (caseInsensitive && token.toLowerCase == token)
        knownCaseInsensitives.add(token)
    }
    // add the original form
    matcher.add(tokens)
    // add the lexical variations
    lexicalVariationEngine.lexicalVariations(tokens).foreach(matcher.add)
  }
}

class FastBuildState(lexicalVariationEngine: LexicalVariations, caseInsensitive: Boolean,
    knownCaseInsensitives: MutableSet[String], labelToIndex: MutableMap[String, Int]) extends BuildState {
  val intHashTrie = new IntHashTrie(caseInsensitive)

  def getCount: Int = intHashTrie.entriesSize

  protected def add(label: String, tokens: Array[String], overwrite: Boolean): Unit = {
    val index = labelToIndex.getOrElseUpdate(label, labelToIndex.size)

    intHashTrie.add(tokens, index, overwrite)
  }

  def addWithLexicalVariations(label: String, tokens: Array[String], caseInsensitive: Boolean, overwrite: Boolean): Unit = {
    // Keep track of all lower case entities that are single tokens (necessary for case-insensitive matching).
    if (tokens.length == 1) {
      val token = tokens.head
      if (caseInsensitive && token.toLowerCase == token)
        knownCaseInsensitives.add(token)
    }
    // add the original form
    add(label, tokens, overwrite)
    // add the lexical variations // use overwrite
    lexicalVariationEngine.lexicalVariations(tokens).foreach { tokens => add(label, tokens, overwrite) }
  }
}

/**
  * A class that builds a [[org.clulab.sequences.SeparatedLexiconNER SeparatedLexiconNER]]
  *
  * The building performed here works on a text file.  The [[org.clulab.sequences.SeparatedLexiconNER SeparatedLexiconNER]] is also
  * Serializable and can be loaded as an object without the text parsing.
  */
class SlowLexiconNERBuilder() extends LexiconNERBuilder() {

  protected def newMatcher(label: String, caseInsensitive: Boolean): BooleanHashTrie =
    if (LexiconNER.USE_DEBUG)
      new DebugBooleanHashTrie(label, caseInsensitive, internStrings = INTERN_STRINGS)
    else
      new BooleanHashTrie(label, caseInsensitive, internStrings = INTERN_STRINGS)

  override def build(standardKbSources: Seq[StandardKbSource], overrideKbSourcesOpt: Option[Seq[OverrideKbSource]],
      entityValidator: EntityValidator, lexicalVariationEngine: LexicalVariations, useLemmasForMatching: Boolean,
      defaultCaseInsensitive: Boolean): SeparatedLexiconNER = {
    logger.info("Beginning to load the KBs for the rule-based NER...")
    requireUniqueStandardKbSources(standardKbSources)

    // The case sensitivity of the overrideKBs will be taken from the matching regular KBs.
    // See https://stackoverflow.com/questions/5042878/how-can-i-convert-immutable-map-to-mutable-map-in-scala.
    val labels = standardKbSources.map(_.getLabel)
    val caseInsensitiveMatchings = standardKbSources.map(_.getCaseInsensitiveMatching)
    // We need to know the order so that the overrideKBs can be sorted the same way as the regular ones.
    val orderMap = MutableMap(labels.zipWithIndex: _*)
    val caseInsensitiveMap = Map(labels.zip(caseInsensitiveMatchings): _*).withDefaultValue(defaultCaseInsensitive)

    val buildState = new SlowBuildState(lexicalVariationEngine)
    val overrideMatchers = getOverrideMatchers(overrideKbSourcesOpt, buildState, caseInsensitiveMap, orderMap)
    val standardMatchers = getStandardMatchers(standardKbSources, buildState, caseInsensitiveMap)

    logger.info("KB loading completed.")
    // Put overrideMatchers first so they take priority during matching.
    new SeparatedLexiconNER((overrideMatchers ++ standardMatchers).toArray, toSet(buildState.knownCaseInsensitives),
        useLemmasForMatching, entityValidator)

    // So, label priority is determined by the order of the standard matchers, which is taken from the kbs argument.
    // First come the override matchers in the same order as the kbs, then any override matchers that don't have
    // a standard matcher, in the order they are encountered, and lastly the standard matchers in their order.
    // If the same NE appears twice in the same KB, it doesn't matter, since the label and case sensitivity
    // will be the same.  If the NE is duplicated in the overrideKBs with the same label, it doesn't matter, either.
    // If there is a duplicate NE with a different label, the label priority determines precedence, not the ordre
    // in which the NE was encountered in the one or more files.  This matches original behavior.
  }

  private def getStandardMatchers(standardKbSources: Seq[StandardKbSource], buildState: SlowBuildState,
      caseInsensitiveMap: Map[String, Boolean]): Seq[BooleanHashTrie] = {
    standardKbSources.map { standardKbSource =>
      val label = standardKbSource.getLabel
      val caseInsensitive = caseInsensitiveMap(label)
      val matcher = newMatcher(label, caseInsensitive)

      standardKbSource.withTokens { tokens: Array[String] =>
        buildState.addWithLexicalVariations(matcher, tokens)
      }
      logger.info(s"Loaded matcher for label $label. The size of the first layer is ${matcher.entriesSize}.")
      matcher
    }
  }

  private def getOverrideMatchers(overrideKbSourcesOpt: Option[Seq[OverrideKbSource]], buildState: SlowBuildState,
      caseInsensitiveMap: Map[String, Boolean], orderMap: MutableMap[String, Int]): Seq[BooleanHashTrie] = {
    overrideKbSourcesOpt.map { overrideKbSources =>
      val matchersArray = new ArrayBuffer[BooleanHashTrie]
      overrideKbSources.foreach { overrideKbSource =>
        // Each overrideKB gets its own copies of these now.
        val matchersMap = new MutableHashMap[String, BooleanHashTrie]()
        val tmpMatchersArray = new ArrayBuffer[BooleanHashTrie]

        overrideKbSource.withLabelAndTokens { case (label, tokens) =>
          val matcher = matchersMap.getOrElseUpdate(label, {
            orderMap.getOrElseUpdate(label, orderMap.size)
            val matcher = new BooleanHashTrie(label, caseInsensitiveMap(label))
            matchersArray += matcher
            matcher
          })

          buildState.addWithLexicalVariations(matcher, tokens)
        }
        // These are no longer sorted alphabetically.
        matchersArray ++= tmpMatchersArray
      }

      matchersArray.foreach { matcher =>
        logger.info(s"Loaded OVERRIDE matcher for label ${matcher.label}.  The size of the first layer is ${matcher.entriesSize}.")
      }
      matchersArray
    }.getOrElse(Seq.empty[BooleanHashTrie])
  }
}

/**
  * A class that builds either a
  * <ul>
  *   <li>[[org.clulab.sequences.CombinedLexiconNER CombinedLexiconNER]] or</li>
  *   <li>[[org.clulab.sequences.CompactLexiconNER CompactLexiconNER]]
  * </ul>
  * depending on the value of useCompact.
  *
  * The building performed here works on a text file.  Both kinds of NERs are also
  * Serializable and can be loaded as objects without the text parsing.
  *
  * @constructor create a new NER that combines its KBs into a small number of tries for fewer lookups, making it fast
  * @param useCompact use the compact representation of the combined NER, which is faster to load from disk once serialized
  */
class FastLexiconNERBuilder(val useCompact: Boolean) extends LexiconNERBuilder() {

  protected def newMatcher(label: String, caseInsensitive: Boolean) =
      new IntHashTrie(caseInsensitive = caseInsensitive, internStrings = INTERN_STRINGS)

  // The Strings here are for filenames.
  override def build(standardKbSources: Seq[StandardKbSource], overrideKbSourcesOpt: Option[Seq[OverrideKbSource]],
      entityValidator: EntityValidator, lexicalVariationEngine: LexicalVariations, useLemmasForMatching: Boolean,
      defaultCaseInsensitive: Boolean): LexiconNER = {
    logger.info("Beginning to load the KBs for the rule-based NER...")
    requireUniqueStandardKbSources(standardKbSources)
    val knownCaseInsensitives = new MutableHashSet[String]()

    // The case sensitivity of the overrideKBs will be taken from the matching regular KBs.
    // We do know the case sensitivity for labels that we haven't seen, devaultCaseInsensitive,
    // but we won't similarly know the label index for new lables, because they are dependent
    // See https://stackoverflow.com/questions/5042878/how-can-i-convert-immutable-map-to-mutable-map-in-scala.
    val labels = standardKbSources.map(_.getLabel)
    val caseInsensitiveMatchings = standardKbSources.map(_.getCaseInsensitiveMatching)
    val labelToIndex = MutableMap(labels.zipWithIndex: _*)
    val caseInsensitiveMap = Map(labels.zip(caseInsensitiveMatchings): _*).withDefaultValue(defaultCaseInsensitive)

    val caseInsensitiveBuildState = new FastBuildState(lexicalVariationEngine, caseInsensitive = true, knownCaseInsensitives, labelToIndex)
    val caseSensitiveBuildState = new FastBuildState(lexicalVariationEngine, caseInsensitive = false, knownCaseInsensitives, labelToIndex)
    addOverrideKBs(overrideKbSourcesOpt, caseInsensitiveBuildState, caseSensitiveBuildState, caseInsensitiveMap)
    addStandardKBs(standardKbSources, caseInsensitiveBuildState, caseSensitiveBuildState, caseInsensitiveMap)
    val labelsWithOverrides = labelToIndex.toArray.sortBy(_._2).map(_._1)

    logger.info("KB loading completed.")

    if (useCompact)
      // These intHashTries need to be in the right order.
      CompactLexiconNER(caseInsensitiveBuildState.intHashTrie, caseSensitiveBuildState.intHashTrie, labelsWithOverrides, toSet(knownCaseInsensitives), useLemmasForMatching, entityValidator)
    else
      CombinedLexiconNER(caseInsensitiveBuildState.intHashTrie, caseSensitiveBuildState.intHashTrie, labelsWithOverrides, toSet(knownCaseInsensitives), useLemmasForMatching, entityValidator)

    // So, these will also be ordered like the slow ones above if there are no duplicates with conflicting case sensitivity.
    // The labels are arranged so that the lowest indexes come from the kb order and then anything in the overrideKBs
    // in the order they are encountered.  As the overrideKBs are read in, replacement is only done if the label index has
    // a lower value, meaning the KB (label) came sooner in the list.  As the regular KBs are read, they never replace
    // anything already present, which is either an overrideKB or a KB earlier in the list with a lower index.
    // If there is a duplicate in the overrides with conflicting case sensitivity, then there can be two copies
    // in the two build states.  They will have different labels, because each label can only have one case sensitivity.
    // Both NERs are searched each time and if the lengths are equal, the one with the lower index is chosen, which
    // should match the result of the slow version.
  }

  private def addStandardKBs(standardKbSources: Seq[StandardKbSource], caseInsensitiveBuildState: FastBuildState,
      caseSensitiveBuildState: FastBuildState, caseInsensitiveMap: Map[String, Boolean]): Unit = {
    standardKbSources.foreach { standardKbSource =>
      val label = standardKbSource.getLabel
      val caseInsensitive = caseInsensitiveMap(label)
      val buildState = if (caseInsensitive) caseInsensitiveBuildState else caseSensitiveBuildState
      val beforeCount = buildState.getCount

      standardKbSource.withTokens { tokens: Array[String] =>
        buildState.addWithLexicalVariations(label, tokens, caseInsensitive, overwrite = false)
      }

      var afterCount = buildState.getCount
      logger.info(s"Loaded matcher for label $label. The number of entries added to the first layer was ${afterCount - beforeCount}.")
    }
  }

  private def addOverrideKBs(overrideKbSourcesOpt: Option[Seq[OverrideKbSource]], caseInsensitiveBuildState: FastBuildState,
      caseSensitiveBuildState: FastBuildState, caseInsensitiveMap: Map[String, Boolean]): Unit = {
    overrideKbSourcesOpt.foreach { overrideKbSources =>
      overrideKbSources.foreach { overrideKbSource =>
        val beforeCount = caseInsensitiveBuildState.getCount + caseSensitiveBuildState.getCount

        overrideKbSource.withLabelAndTokens { case (label, tokens) =>
          val caseInsensitive = caseInsensitiveMap(label)
          val buildState = if (caseInsensitive) caseInsensitiveBuildState else caseSensitiveBuildState

          buildState.addWithLexicalVariations(label, tokens, caseInsensitive, overwrite = true)
        }

        var afterCount = caseInsensitiveBuildState.getCount + caseSensitiveBuildState.getCount
        logger.info(s"Loaded OVERRIDE matchers for all labels.  The number of entries added to the first layer was ${afterCount - beforeCount}.")
      }
    }
  }
}

// This is presently not related to any other classes, but as the name suggests, might
// eventually extend the StandardKbSource to add processing of the comments.
object CommentedStandardKbSource {
  val COMMENT = "//"
  val COMMENT_LENGTH = COMMENT.length

  def read(source: Source)(f: (String, Option[String]) => Unit): Unit = {
    val lines = FileUtils.getCommentedLinesFromSource(source)

    lines.foreach { line =>
      val commentStart = line.indexOf(COMMENT)

      if (commentStart < 0)
        f(line.trim, None)
      else {
        val code = line.substring(0, commentStart).trim
        val comment = line.substring(commentStart + COMMENT_LENGTH).trim

        f(code, Some(comment))
      }
    }
  }
}
