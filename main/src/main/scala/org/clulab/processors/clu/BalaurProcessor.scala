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
  wordTokenizer: Tokenizer,
  wordLemmatizer: Lemmatizer,
  tokenClassifier: TokenClassifier // multi-task classifier for all tasks addressed
) extends Processor with Configured {

  // standard, abbreviated constructor
  def this(
    config: Config = ConfigFactory.load("balaurprocessor"),
    optionalNER: Option[LexiconNER] = None,
    seasonPathOpt: Option[String] = None
  ) = this(config, 
           optionalNER, 
           CluProcessor.newNumericEntityRecognizerOpt(seasonPathOpt), 
           mkTokenizer(BalaurProcessor.getArgString(config, s"$prefix.language", Some("EN"))),
           mkLemmatizer(BalaurProcessor.getArgString(config, s"$prefix.language", Some("EN"))),
           TokenClassifier.fromFiles(config.getString(s"$prefix.modelName")))

  override def getConf: Config = config

  override def mkDocument(text: String, keepText: Boolean): Document = { 
    DocumentMaker.mkDocument(wordTokenizer, text, keepText)
  }

  override def mkDocumentFromSentences(sentences: Iterable[String], 
    keepText: Boolean, 
    charactersBetweenSentences: Int): Document = {     
    DocumentMaker.mkDocumentFromSentences(wordTokenizer, sentences, keepText, charactersBetweenSentences)
  }

  override def mkDocumentFromTokens(sentences: Iterable[Iterable[String]], 
    keepText: Boolean, 
    charactersBetweenSentences: Int, 
    charactersBetweenTokens: Int): Document = { 
    DocumentMaker.mkDocumentFromTokens(sentences, keepText, charactersBetweenSentences, charactersBetweenSentences)
  }

  override def tagPartsOfSpeech(doc: Document): Unit = {
    throw new RuntimeException("ERROR: cannot call this method on its in this procecessor!")
  }

  override def lemmatize(doc: Document): Unit = {}

  override def recognizeNamedEntities(doc: Document): Unit = {
    throw new RuntimeException("ERROR: cannot call this method on its own in this procecessor!")
  }

  override def parse(doc: Document): Unit = {
    throw new RuntimeException("ERROR: cannot call this method on its own in this procecessor!")
  }

  override def srl(doc: Document): Unit = {
    throw new RuntimeException("ERROR: functionality not supported in this procecessor!")
  }

  override def chunking(doc: Document): Unit = {
    throw new RuntimeException("ERROR: cannot call this method on its own in this procecessor!")
  }

  override def resolveCoreference(doc: Document): Unit = {
    throw new RuntimeException("ERROR: functionality not supported in this procecessor!")
  }

  override def discourse(doc: Document): Unit = {
    throw new RuntimeException("ERROR: functionality not supported in this procecessor!")
  }

  override def relationExtraction(doc: Document): Unit = {
    throw new RuntimeException("ERROR: functionality not supported in this procecessor!")
  }

  override def annotate(doc: Document): Document = {
    for(sent <- doc.sentences) {
      val allLabels = tokenClassifier.predict(sent.words)
      for (labels <- allLabels) {
        if(labels != null) {
          println(s"Labels: ${labels.mkString(", ")}")
        }
      }
    }
    doc
  }

}

object BalaurProcessor {
  val logger:Logger = LoggerFactory.getLogger(classOf[CluProcessor])
  val prefix:String = "BalaurProcessor"

  val OUTSIDE = "O"

  def mkTokenizer(lang: String): Tokenizer = {
    lang match {
      case "PT" => new OpenDomainPortugueseTokenizer
      case "ES" => new OpenDomainSpanishTokenizer
      case _ => new OpenDomainEnglishTokenizer
    }
  }

  def mkLemmatizer(lang: String): Lemmatizer = {
    lang match {
      case "PT" => new PortugueseLemmatizer
      case "ES" => new SpanishLemmatizer
      case _ => new EnglishLemmatizer
    }
  }

  def getArgString (config: Config, argPath: String, defaultValue: Option[String]): String =
    if (config.hasPath(argPath)) config.getString(argPath)
    else if(defaultValue.nonEmpty) defaultValue.get
    else throw new RuntimeException(s"ERROR: parameter $argPath must be defined!")
}