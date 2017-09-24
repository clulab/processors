package org.clulab.processors.bionlp.ner

import java.io.BufferedReader

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import com.typesafe.config._
import ai.lum.common.ConfigUtils._

import org.clulab.utils.Files._

import org.clulab.struct.HashTrie
import org.slf4j.LoggerFactory

class KBLoader

/**
  * Loads the KBs from bioresources under org/clulab/reach/kb/ner
  * These must be generated offline by KBGenerator; see bioresources/ner_kb.sh
  * User: mihais. 2/7/16.
  * Last Modified: Update to use external configuration file.
  */
object KBLoader {
  val config = ConfigFactory.load()         // load the configuration file

  private val logger = LoggerFactory.getLogger(classOf[KBLoader])
  private val lock = new KBLoader // to be used for the singleton in loadAll

  val NAME_FIELD_NDX = 0                    // where the text of the NE is specified
  val LABEL_FIELD_NDX = 4                   // where the label of the NE is specified

  /** List of entity labeling files for the rule-based NER. If missing, an error is thrown.
    * NB: file order is important: it indicates priority! */
  val RULE_NER_KBS: List[String] = config[List[String]]("kbloader.nerKBs")
  logger.debug(s"KBLoader.init): RULE_NER_KBS=${RULE_NER_KBS}")

  /** List of KB override files to be used. */
  val NER_OVERRIDE_KBS: List[String] =
    if (config.hasPath("kbloader.overrides")) config[List[String]]("kbloader.overrides")
    else List.empty[String]
  logger.debug(s"KBLoader.init): NER_OVERRIDE_KBS=${NER_OVERRIDE_KBS}")

  /** These must be KBs BEFORE KBGenerator converts them to NER-ready, because
    * the files under kb/ner are post tokenization. */
  private val unslashable =
    if (config.hasPath("kbloader.unslashables")) config[List[String]]("kbloader.unslashables")
    else List.empty[String]
  val UNSLASHABLE_TOKENS_KBS: List[String] = NER_OVERRIDE_KBS ++ unslashable
  logger.debug(s"KBLoader.init): UNSLASHABLE_TOKENS_KBS=${UNSLASHABLE_TOKENS_KBS}")

  /** A horrible hack to keep track of entities that should not be labeled when in
    * lower case, or upper initial case. */
  private val stopListFile: Option[String] =
    if (config.hasPath("kbloader.stopListFile")) Option(config[String]("kbloader.stopListFile"))
    else None
  val ENTITY_STOPLIST: Set[String] =
    if (stopListFile.isDefined) loadEntityStopList(stopListFile.get)
    else Set.empty[String]
  logger.debug(s"KBLoader.init): ENTITY_STOPLIST=${ENTITY_STOPLIST}")

  /** Engine to automatically produce lexical variations of entity names */
  val lexicalVariationEngine = Some(new LexicalVariations)

  def loadEntityStopList(kb:String):Set[String] = {
    val stops = new mutable.HashSet[String]()
    val reader = loadStreamFromClasspath(kb)
    var done = false
    while(! done) {
      val line = reader.readLine()
      if(line == null) {
        done = true
      } else {
        val l = line.trim
        if(! l.isEmpty && ! l.startsWith("#")) {
          stops += l
        }
      }
    }
    reader.close()
    stops.toSet
  }

  // Load the rule NER just once, so multiple processors can share it
  var ruleNerSingleton: Option[RuleNER] = None

  def loadAll:RuleNER = {
    lock.synchronized {
      if(ruleNerSingleton.isEmpty) {
        ruleNerSingleton = Some(load(RULE_NER_KBS,
          Some(NER_OVERRIDE_KBS), // allow overriding for some key entities
          useLemmas = false,
          caseInsensitive = true))
      }
      ruleNerSingleton.get
    }
  }

  /**
    * Loads all KBs; KBs must be listed in descending order of their priorities
    */
  def load (
    kbs:List[String],
    overrideKBs:Option[List[String]],
    useLemmas:Boolean = false,
    caseInsensitive:Boolean = true
  ): RuleNER = {
    logger.info("Beginning to load the KBs for the rule-based bio NER...")
    val matchers = new ArrayBuffer[(String, HashTrie)]
    val knownCaseInsensitives = new mutable.HashSet[String]()

    // load the override KBs first, so they take priority during matching
    if (overrideKBs.isDefined) {
      overrideKBs.get.foreach(okb => {
        val reader = loadStreamFromClasspath(okb)
        val overrideMatchers = loadOverrideKB(reader, caseInsensitive, knownCaseInsensitives)
        for(name <- overrideMatchers.keySet.toList.sorted) {
          val matcher = overrideMatchers(name)
          logger.info(s"Loaded OVERRIDE matcher for label $name. This matcher contains ${matcher.uniqueStrings.size} unique strings; the size of the first layer is ${matcher.entries.size}.")
          matchers += Tuple2(name, matcher)
        }
        reader.close()
      })
    }

    // load the standard KBs
    for(kb <- kbs) {
      val name = extractKBName(kb)
      val reader = loadStreamFromClasspath(kb)
      val matcher = loadKB(reader, caseInsensitive, knownCaseInsensitives)
      logger.info(s"Loaded matcher for label $name. This matcher contains ${matcher.uniqueStrings.size} unique strings; the size of the first layer is ${matcher.entries.size}.")
      matchers += Tuple2(name, matcher)
      reader.close()
    }

    logger.info("KB loading completed.")
    new RuleNER(matchers.toArray, knownCaseInsensitives.toSet, useLemmas)
  }
  
  private def loadOverrideKB(
    reader:BufferedReader,
    caseInsensitive:Boolean,
    knownCaseInsensitives:mutable.HashSet[String]): Map[String, HashTrie] = {
    val matchers = new mutable.HashMap[String, HashTrie]()
    var done = false
    while(! done) {
      val line = reader.readLine()
      if(line == null) {
        done = true
      } else {
        addOverrideLine(line, matchers, caseInsensitive, knownCaseInsensitives)
      }
    }
    matchers.toMap
  }

  private def addOverrideLine(
    inputLine:String,
    matchers:mutable.HashMap[String, HashTrie],
    caseInsensitive:Boolean,
    knownCaseInsensitives:mutable.HashSet[String]
  ): Unit = {
    val line = inputLine.trim
    if(! line.startsWith("#")) { // skip comments starting with #
      val blocks = line.split("\t")
      if (blocks.size >= (1 + LABEL_FIELD_NDX)) {
        val entity = blocks(NAME_FIELD_NDX)   // grab the text of the named entity
        val label = blocks(LABEL_FIELD_NDX)   // grab the label of the named entity

        val tokens = entity.split("\\s+")
        // keep track of all lower case ents that are single letter
        if (tokens.length == 1 && line.toLowerCase == line) {
          knownCaseInsensitives.add(line)
        }
        val matcher = matchers.getOrElseUpdate(label,
          new HashTrie(caseInsensitive = caseInsensitive, internStrings = true))

        addWithLexicalVariations(tokens, matcher)

      }
    }
  }

  private def addWithLexicalVariations(tokens:Array[String], matcher:HashTrie): Unit = {
    // add the original form
    matcher.add(tokens)

    // add the lexical variations
    for(ts <- KBLoader.lexicalVariations(tokens)) {
      matcher.add(ts)
    }
  }

  private def addLine(inputLine:String, matcher:HashTrie, knownCaseInsensitives:mutable.HashSet[String]): Unit = {
    val line = inputLine.trim
    if(! line.startsWith("#")) {
      val tokens = line.split("\\s+")
      addWithLexicalVariations(tokens, matcher)
      if(tokens.length == 1 && line.toLowerCase == line) { // keep track of all lower case ents that are single letter
        knownCaseInsensitives.add(line)
      }
    }
  }

  private def loadKB(
    reader:BufferedReader,
    caseInsensitive:Boolean,
    knownCaseInsensitives:mutable.HashSet[String]): HashTrie = {
    val matcher = new HashTrie(caseInsensitive = caseInsensitive, internStrings = true)
    var done = false
    while(! done) {
      val line = reader.readLine()
      if(line == null) {
        done = true
      } else {
        addLine(line, matcher, knownCaseInsensitives)
      }
    }
    matcher
  }

  private def extractKBName(kb:String):String = {
    val slash = kb.lastIndexOf("/")
    val dot = kb.indexOf('.')
    val name = kb.substring(slash + 1, dot)
    name
  }

  /**
    * Generates all accepted lexical variations for this entity
    *   For example: "insulin receptor substrate 1" => "insulin receptor substrate-1"
    *
    * @param tokens The original form of the entity
    * @return All accepted lexical variations, including the original form
    */
  def lexicalVariations(tokens:Array[String]):Seq[Array[String]] = {
    lexicalVariationEngine match {
      case Some(e) => e.lexicalVariations(tokens)
      case _ => Seq.empty
    }
  }
}
