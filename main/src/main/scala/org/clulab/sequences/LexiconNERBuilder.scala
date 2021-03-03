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
import org.clulab.utils.Files.loadStreamFromClasspath
import org.clulab.utils.Serializer
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.{
  HashMap => MutableHashMap,
  HashSet => MutableHashSet,
  Map => MutableMap,
  Set => MutableSet
}
import scala.collection.mutable.ArrayBuffer

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

  // The kbs and caseInsensitiveMatchines sequences should be parallel.
  def build(kbs: Seq[String], caseInsensitiveMatchings: Seq[Boolean], overrideKBs: Option[Seq[String]],
      entityValidator: EntityValidator, lexicalVariationEngine: LexicalVariations, useLemmasForMatching: Boolean,
      // This default value is used when there are overrideKBs with labels that don't match a regular KB.
      defaultCaseInsensitive: Boolean): LexiconNER

  protected def toJavaConsumer[T](consumer: T => Unit): Consumer[T] = {
    (t: T) => consumer(t)
  }

  protected def extractKBName(kb: String): String = {
    val slash = kb.lastIndexOf("/")
    val dot = kb.indexOf('.')
    val name = kb.substring(slash + 1, dot)
    name
  }

  protected def readLines(path: String)(addLine: String => Unit): Unit = {
    Serializer.using(loadStreamFromClasspath(path)) { reader =>
      reader.lines.forEach(toJavaConsumer[String] { line: String =>
        addLine(line)
      })
    }
  }

  // Convert the mutable set into an immutable one consistently.
  protected def toSet(mutableSet: MutableSet[String]): Set[String] = {
    // In the end it was a different set causing problems.
    // val strings = mutableSet.toArray.sorted
    // Set(strings: _*)
    mutableSet.toSet
  }
}

/**
  * A class that builds a [[org.clulab.sequences.SeparatedLexiconNER SeparatedLexiconNER]]
  *
  * The building performed here works on a text file.  The [[org.clulab.sequences.SeparatedLexiconNER SeparatedLexiconNER]] is also
  * Serializable and can be loaded as an object without the text parsing.
  */
class SlowLexiconNERBuilder() extends LexiconNERBuilder() {

  class BuildState(lexicalVariationEngine: LexicalVariations) {
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

  protected def newMatcher(label: String, caseInsensitive: Boolean): BooleanHashTrie =
    if (LexiconNER.USE_DEBUG)
      new DebugBooleanHashTrie(label, caseInsensitive, internStrings = INTERN_STRINGS)
    else
      new BooleanHashTrie(label, caseInsensitive, internStrings = INTERN_STRINGS)

  override def build(kbs: Seq[String], caseInsensitiveMatchings: Seq[Boolean], overrideKBs: Option[Seq[String]],
      entityValidator: EntityValidator, lexicalVariationEngine: LexicalVariations, useLemmasForMatching: Boolean,
      defaultCaseInsensitive: Boolean): SeparatedLexiconNER = {
    logger.info("Beginning to load the KBs for the rule-based NER...")

    // The case sensitivity of the overrideKBs will be taken from the matching regular KBs.
    // See https://stackoverflow.com/questions/5042878/how-can-i-convert-immutable-map-to-mutable-map-in-scala.
    val labels = kbs.map(extractKBName)
    require(labels.distinct.length == labels.length, "KB labels are repeated!")
    // We need to know the order so that the overrideKBs can be sorted the same way as the regular ones.
    val orderMap = MutableMap(labels.zipWithIndex: _*)
    val caseInsensitiveMap = Map(labels.zip(caseInsensitiveMatchings): _*).withDefaultValue(defaultCaseInsensitive)

    val buildState = new BuildState(lexicalVariationEngine)
    val overrideMatchers = getOverrideMatchers(overrideKBs, buildState, caseInsensitiveMap, orderMap)
    val standardMatchers = getStandardMatchers(kbs, buildState, caseInsensitiveMap)

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

  private def getStandardMatchers(kbs: Seq[String], buildState: BuildState, caseInsensitiveMap: Map[String, Boolean]): Seq[BooleanHashTrie] = {
    kbs.map { kb =>
      val label = extractKBName(kb)
      val caseInsensitive = caseInsensitiveMap(label)
      val matcher = newMatcher(label, caseInsensitive)

      readLines(kb) { inputLine =>
        val line = inputLine.trim
        if (!line.startsWith("#")) {
          val tokens = line.split("\\s+")
          buildState.addWithLexicalVariations(matcher, tokens)
        }
      }

      logger.info(s"Loaded matcher for label $label. The size of the first layer is ${matcher.entriesSize}.")
      matcher
    }
  }

  private def getOverrideMatchers(overrideKBsOpt: Option[Seq[String]], buildState: BuildState, caseInsensitiveMap: Map[String, Boolean], orderMap: MutableMap[String, Int]): Seq[BooleanHashTrie] = {
    overrideKBsOpt.map { overrideKBs =>
      val matchersMap = new MutableHashMap[String, BooleanHashTrie]()
      // This array is used to keep track of the order in which matchers are added for the sake of consistency.
      val matchersArray = new ArrayBuffer[BooleanHashTrie]

      // In these KBs, the name of the entity must be the first token, and the label must be the last.
      // Tokenization is performed around TABs here.
      def addLine(inputLine: String): Unit = {
        val line = inputLine.trim
        if (!line.startsWith("#")) { // skip comments starting with #
          val blocks = line.split("\t")
          if (blocks.size >= 2) {
            val entity = blocks.head // grab the text of the named entity
            val label = blocks.last // grab the label of the named entity
            val tokens = entity.split("\\s+")
            val matcher = matchersMap.getOrElseUpdate(label, {
              orderMap(label) = orderMap.size // It won't be here, either.
              val matcher = new BooleanHashTrie(label, caseInsensitiveMap(label))
              matchersArray += matcher
              matcher
            })

            buildState.addWithLexicalVariations(matcher, tokens)
          }
        }
      }

      overrideKBs.foreach { overrideKB =>
        readLines(overrideKB)(addLine)
      }

      // The order should be all the withRegular first, in that regular order,
      // and then the ones withoutRegular in the order they were encountered.
      val (withRegular, withoutRegular) = matchersArray.partition { matcher =>
        caseInsensitiveMap.contains(matcher.label)
      }
      val sortedRegular = withRegular.sortBy { matcher => orderMap(matcher.label) }
      val sortedMatchers = sortedRegular ++ withoutRegular

      sortedMatchers.foreach { matcher =>
        logger.info(s"Loaded OVERRIDE matcher for label ${matcher.label}.  The size of the first layer is ${matcher.entriesSize}.")
      }
      sortedMatchers
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

  class BuildState(lexicalVariationEngine: LexicalVariations, caseInsensitive: Boolean,
      knownCaseInsensitives: MutableSet[String], labelToIndex: MutableMap[String, Int]) {
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

  protected def newMatcher(label: String, caseInsensitive: Boolean) =
      new IntHashTrie(caseInsensitive = caseInsensitive, internStrings = INTERN_STRINGS)

  override def build(kbs: Seq[String], caseInsensitiveMatchings: Seq[Boolean], overrideKBs: Option[Seq[String]],
      entityValidator: EntityValidator, lexicalVariationEngine: LexicalVariations, useLemmasForMatching: Boolean,
      defaultCaseInsensitive: Boolean): LexiconNER = {
    logger.info("Beginning to load the KBs for the rule-based NER...")
    val knownCaseInsensitives = new MutableHashSet[String]()

    // The case sensitivity of the overrideKBs will be taken from the matching regular KBs.
    // We do know the case sensitivity for labels that we haven't seen, devaultCaseInsensitive,
    // but we won't similarly know the label index for new lables, because they are dependent
    // See https://stackoverflow.com/questions/5042878/how-can-i-convert-immutable-map-to-mutable-map-in-scala.
    val labels = kbs.map(extractKBName)
    require(labels.distinct.length == labels.length, "KB labels are repeated!")
    // We need to know the order so that the overrideKBs can be sorted the same way as the regular ones.
    val labelToIndex = MutableMap(labels.zipWithIndex: _*)
    val caseInsensitiveMap = Map(labels.zip(caseInsensitiveMatchings): _*).withDefaultValue(defaultCaseInsensitive)

    val caseInsensitiveBuildState = new BuildState(lexicalVariationEngine, caseInsensitive = true, knownCaseInsensitives, labelToIndex)
    val caseSensitiveBuildState = new BuildState(lexicalVariationEngine, caseInsensitive = false, knownCaseInsensitives, labelToIndex)
    addOverrideKBs(overrideKBs, caseInsensitiveBuildState, caseSensitiveBuildState, caseInsensitiveMap)
    addStandardKBs(kbs, caseInsensitiveBuildState, caseSensitiveBuildState, caseInsensitiveMap)
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

  private def addStandardKBs(kbs: Seq[String], caseInsensitiveBuildState: BuildState,
      caseSensitiveBuildState: BuildState, caseInsensitiveMap: Map[String, Boolean]): Unit = {
    kbs.foreach { kb =>
      val label = extractKBName(kb)
      val caseInsensitive = caseInsensitiveMap(label)
      val buildState = if (caseInsensitive) caseInsensitiveBuildState else caseSensitiveBuildState
      val beforeCount = buildState.getCount

      readLines(kb) { inputLine =>
        val line = inputLine.trim
        if (!line.startsWith("#")) {
          val tokens = line.split("\\s+")
          buildState.addWithLexicalVariations(label, tokens, caseInsensitive, overwrite = false)
        }
      }

      var afterCount = buildState.getCount
      logger.info(s"Loaded matcher for label $label. The number of entries added to the first layer was ${afterCount - beforeCount}.")
    }
  }

  private def addOverrideKBs(overrideKBsOpt: Option[Seq[String]], caseInsensitiveBuildState: BuildState,
      caseSensitiveBuildState: BuildState, caseInsensitiveMap: Map[String, Boolean]): Unit = {
    overrideKBsOpt.foreach { overrideKBs =>
      // In these KBs, the name of the entity must be the first token, and the label must be the last.
      // Tokenization is performed around TABs here.
      def addLine(inputLine: String): Unit = {
        val line = inputLine.trim
        if (!line.startsWith("#")) { // skip comments starting with #
          val blocks = line.split("\t")
          if (blocks.size >= 2) {
            val entity = blocks.head // grab the text of the named entity
            val label = blocks.last // grab the label of the named entity
            val tokens = entity.split("\\s+")

            val caseInsensitive = caseInsensitiveMap(label)
            val buildState = if (caseInsensitive) caseInsensitiveBuildState else caseSensitiveBuildState
            buildState.addWithLexicalVariations(label, tokens, caseInsensitive, overwrite = true)
          }
        }
      }

      overrideKBsOpt.getOrElse(Seq.empty).foreach { overrideKB =>
        val beforeCount = caseInsensitiveBuildState.getCount + caseSensitiveBuildState.getCount
        readLines(overrideKB)(addLine)
        var afterCount = caseInsensitiveBuildState.getCount + caseSensitiveBuildState.getCount
        logger.info(s"Loaded OVERRIDE matchers for all labels.  The number of entries added to the first layer was ${afterCount - beforeCount}.")
      }
    }
  }
}
