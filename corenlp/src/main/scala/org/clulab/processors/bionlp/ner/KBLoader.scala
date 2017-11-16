package org.clulab.processors.bionlp.ner

import com.typesafe.config._
import ai.lum.common.ConfigUtils._
import org.clulab.processors.clu.bio.{BioLexicalVariations, BioLexiconEntityValidator}
import org.clulab.sequences.LexiconNER
import org.slf4j.LoggerFactory
import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import org.clulab.utils.Files

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
  // logger.debug(s"KBLoader.init): RULE_NER_KBS=$RULE_NER_KBS")

  /** List of KB override files to be used. */
  val NER_OVERRIDE_KBS: List[String] =
    if (config.hasPath("kbloader.overrides")) config[List[String]]("kbloader.overrides")
    else List.empty[String]
  // logger.debug(s"KBLoader.init): NER_OVERRIDE_KBS=$NER_OVERRIDE_KBS")

  /** These must be KBs BEFORE KBGenerator converts them to NER-ready, because
    * the files under kb/ner are post tokenization. */
  private val unslashable =
    if (config.hasPath("kbloader.unslashables")) config[List[String]]("kbloader.unslashables")
    else List.empty[String]
  val UNSLASHABLE_TOKENS_KBS: List[String] = NER_OVERRIDE_KBS ++ unslashable
  // logger.debug(s"KBLoader.init): UNSLASHABLE_TOKENS_KBS=$UNSLASHABLE_TOKENS_KBS")

  /** A horrible hack to keep track of entities that should not be labeled when in
    * lower case, or upper initial case. */
  val stopListFile: Option[String] =
    if (config.hasPath("kbloader.stopListFile")) Option(config[String]("kbloader.stopListFile"))
    else None

  val serNerModel: Option[String] =
    if(config.hasPath("kbloader.nerSerModel")) Option(config[String]("kbloader.nerSerModel"))
    else None

  // Load the rule NER just once, so multiple processors can share it
  var ruleNerSingleton: Option[LexiconNER] = None

  def loadAll(fromSerializedModel:Boolean = false):LexiconNER = {
    lock.synchronized {
      if(ruleNerSingleton.isEmpty) {

        // try the serialized model first
        if(fromSerializedModel && serNerModel.nonEmpty) {
          logger.debug(s"Loading LexiconNER from serialized model: ${serNerModel.get}")
          val ois = Files.loadObjectStreamFromClasspath(serNerModel.get)
          ruleNerSingleton = Some(ois.readObject().asInstanceOf[LexiconNER])
          ois.close()
          val labels = ruleNerSingleton.get.matchers.map(_._1).sorted
          logger.debug(s"Loaded tries for ${labels.size} labels (repeated labels are due to the override KB): ${labels.mkString(", ")}")
          logger.debug("Completed NER loading.")
        }

        if(ruleNerSingleton.isEmpty) {
          logger.debug("Loading LexiconNER from knowledge bases...")
          ruleNerSingleton = Some(LexiconNER(
            RULE_NER_KBS,
            Some(NER_OVERRIDE_KBS), // allow overriding for some key entities
            new BioLexiconEntityValidator,
            new BioLexicalVariations,
            useLemmasForMatching = false,
            caseInsensitiveMatching = true
          ))
          logger.debug("Completed NER loading.")
        }
      }
      ruleNerSingleton.get
    }
  }

  /**
    * Creates the serialized LexiconNER model from the provided KBs
    * This is called in bioresources/ner_kb.sh
    * @param args The file in which to save the serialized model
    */
  def main(args:Array[String]): Unit = {
    val modelFile = args(0)
    val ner = loadAll(fromSerializedModel = false)

    val oos = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(modelFile)))
    oos.writeObject(ner)
    oos.close()
  }
}
