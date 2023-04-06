package org.clulab.processors.clu

import org.clulab.scala_transformers.encoder.TokenClassifier
import com.typesafe.config.Config

import org.clulab.processors.clu.tokenizer._
import org.clulab.sequences.{LexiconNER, NamedEntity}
import org.clulab.numeric.{NumericEntityRecognizer, setLabelsAndNorms}
import org.clulab.processors.{Document, IntermediateDocumentAttachment, Processor, Sentence}
import org.clulab.utils.{BeforeAndAfter, Configured, DependencyUtils, Lazy, ScienceUtils, ToEnhancedDependencies, ToEnhancedSemanticRoles}
import com.typesafe.config.ConfigFactory
import org.slf4j.{Logger, LoggerFactory}

import BalaurProcessor._

class BalaurProcessor protected (
  val config: Config,
  val optionalNER: Option[LexiconNER],
  numericEntityRecognizerOpt: Option[NumericEntityRecognizer],
  internStringsOpt: Option[Boolean],
  localTokenizerOpt: Option[Tokenizer],
  lemmatizerOpt: Option[Lemmatizer],
  tokenClassifier: Option[TokenClassifier]
) extends Processor with Configured {

  // standard, abbreviated constructor
  def this(
    config: Config = ConfigFactory.load("balaurprocessor"),
    optionalNER: Option[LexiconNER] = None,
    seasonPathOpt: Option[String] = None
  ) = this(config, optionalNER, 
            CluProcessor.newNumericEntityRecognizerOpt(seasonPathOpt), None, None, None, 
            Some(TokenClassifier(config.getString(s"$prefix.modelName"))))

  override def getConf: Config = config

  // should we intern strings or not?
  val internStrings:Boolean = internStringsOpt.getOrElse(
    getArgBoolean(s"$prefix.internStrings", Some(false))
  )

  override def mkDocument(text: String, keepText: Boolean): Document = { null }

  override def mkDocumentFromSentences(sentences: Iterable[String], keepText: Boolean, charactersBetweenSentences: Int): Document = { null }

  override def mkDocumentFromTokens(sentences: Iterable[Iterable[String]], keepText: Boolean, charactersBetweenSentences: Int, charactersBetweenTokens: Int): Document = { null }

  override def tagPartsOfSpeech(doc: Document): Unit = {}

  override def lemmatize(doc: Document): Unit = {}

  override def recognizeNamedEntities(doc: Document): Unit = {}

  override def parse(doc: Document): Unit = {}

  override def srl(doc: Document): Unit = {}

  override def chunking(doc: Document): Unit = {}

  override def resolveCoreference(doc: Document): Unit = {}

  override def discourse(doc: Document): Unit = {}

  override def relationExtraction(doc: Document): Unit = {}

  override def annotate(doc: Document): Document = {
    null
  }

}

object BalaurProcessor {
  val logger:Logger = LoggerFactory.getLogger(classOf[CluProcessor])
  val prefix:String = "BalaurProcessor"

  val OUTSIDE = "O"
}