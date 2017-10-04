package org.clulab.processors.bionlp.ner

import scala.collection.mutable
import com.typesafe.config._
import ai.lum.common.ConfigUtils._
import org.clulab.processors.clu.bio.{BioLexicalVariations, BioLexiconEntityValidator}
import org.clulab.sequences.LexiconNER
import org.clulab.utils.Files._
import org.slf4j.LoggerFactory

class KBLoader

/**
  * Loads the KBs from bioresources under org/clulab/reach/kb/ner
  * These must be generated offline by KBGenerator; see bioresources/ner_kb.sh
  * User: mihais. 2/7/16.
  * Last Modified: Update to use external configuration file.
  */
object KBLoader {
  val config: Config = ConfigFactory.load()         // load the configuration file

  private val logger = LoggerFactory.getLogger(classOf[KBLoader])
  private val lock = new KBLoader // to be used for the singleton in loadAll

  /** List of entity labeling files for the rule-based NER. If missing, an error is thrown.
    * NB: file order is important: it indicates priority! */
  val RULE_NER_KBS: List[String] = config[List[String]]("kbloader.nerKBs")
  logger.debug(s"KBLoader.init): RULE_NER_KBS=$RULE_NER_KBS")

  /** List of KB override files to be used. */
  val NER_OVERRIDE_KBS: List[String] =
    if (config.hasPath("kbloader.overrides")) config[List[String]]("kbloader.overrides")
    else List.empty[String]
  logger.debug(s"KBLoader.init): NER_OVERRIDE_KBS=$NER_OVERRIDE_KBS")

  /** These must be KBs BEFORE KBGenerator converts them to NER-ready, because
    * the files under kb/ner are post tokenization. */
  private val unslashable =
    if (config.hasPath("kbloader.unslashables")) config[List[String]]("kbloader.unslashables")
    else List.empty[String]
  val UNSLASHABLE_TOKENS_KBS: List[String] = NER_OVERRIDE_KBS ++ unslashable
  logger.debug(s"KBLoader.init): UNSLASHABLE_TOKENS_KBS=$UNSLASHABLE_TOKENS_KBS")

  /** A horrible hack to keep track of entities that should not be labeled when in
    * lower case, or upper initial case. */
  private val stopListFile: Option[String] =
    if (config.hasPath("kbloader.stopListFile")) Option(config[String]("kbloader.stopListFile"))
    else None
  val ENTITY_STOPLIST: Set[String] =
    if (stopListFile.isDefined) loadEntityStopList(stopListFile.get)
    else Set.empty[String]
  logger.debug(s"KBLoader.init): ENTITY_STOPLIST=$ENTITY_STOPLIST")

  /** Engine to automatically produce lexical variations of entity names */
  val lexicalVariationEngine = Some(new BioLexicalVariations)

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
  var ruleNerSingleton: Option[LexiconNER] = None

  def loadAll:LexiconNER = {
    lock.synchronized {
      if(ruleNerSingleton.isEmpty) {
        ruleNerSingleton = Some(LexiconNER(
          RULE_NER_KBS,
          Some(NER_OVERRIDE_KBS), // allow overriding for some key entities
          new BioLexiconEntityValidator,
          new BioLexicalVariations,
          useLemmasForMatching = false,
          caseInsensitiveMatching = true
        ))
      }
      ruleNerSingleton.get
    }
  }

}
