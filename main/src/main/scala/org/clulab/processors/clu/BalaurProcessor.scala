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
import PostProcessor._
import org.clulab.struct.GraphMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import org.clulab.struct.Edge
import org.clulab.struct.DirectedGraph
import scala.collection.mutable.HashSet

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
    seasonPathOpt: Option[String] = Some("/org/clulab/numeric/SEASON.tsv")
  ) = this(
    config,
    optionalNER,
    newNumericEntityRecognizerOpt(seasonPathOpt),
    mkTokenizer(BalaurProcessor.getArgString(config, s"$prefix.language", Some("EN"))),
    mkLemmatizer(BalaurProcessor.getArgString(config, s"$prefix.language", Some("EN"))),
    // TokenClassifier.fromFiles(config.getString(s"$prefix.modelName"))
    TokenClassifier.fromResources(config.getString(s"$prefix.modelName"))
  )

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

  /** Lematization; modifies the document in place */
  override def lemmatize(doc: Document): Unit = {
    for(sent <- doc.sentences) {
      val lemmas = new Array[String](sent.size)
      for(i <- sent.words.indices) {
        lemmas(i) = wordLemmatizer.lemmatizeWord(sent.words(i))

        // a lemma may be empty in some weird Unicode situations
        if(lemmas(i).isEmpty) {
          logger.debug(s"""WARNING: Found empty lemma for word #$i "${sent.words(i)}" in sentence: ${sent.words.mkString(" ")}""")
          lemmas(i) = sent.words(i).toLowerCase()
        }
      }
      sent.lemmas = Some(lemmas)
    }
  }

  /** Generates cheap lemmas with the word in lower case, for languages where a lemmatizer is not available */
  def cheapLemmatize(doc:Document): Unit = {
    for(sent <- doc.sentences) {
      val lemmas = sent.words.map(_.toLowerCase()).toArray
      sent.lemmas = Some(lemmas)
    }
  }

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
    val verbose = false

    // lemmas are created deterministically, not through the MTL framework
    lemmatize(doc)

    // process one sentence at a time through the MTL framework
    for(sent <- doc.sentences) {
      val allLabels = tokenClassifier.predict(sent.words)
      if(verbose) {
        for (labels <- allLabels) {
          if(labels != null) {
            println(s"Labels: ${labels.mkString(", ")}")
          }
        }
      }
      assignPosTags(allLabels(TASK_TO_INDEX(POS_TASK)), sent)
      assignNamedEntityLabels(allLabels(TASK_TO_INDEX(NER_TASK)), sent)
      assignChunkLabels(allLabels(TASK_TO_INDEX(CHUNKING_TASK)), sent)
      assignDependencyLabels(allLabels(TASK_TO_INDEX(DEPS_HEAD_TASK)), 
        allLabels(TASK_TO_INDEX(DEPS_LABEL_TASK)), sent)
    }

    // numeric entities using our numeric entity recognizer based on Odin rules
    if(numericEntityRecognizerOpt.nonEmpty) {
      val numericMentions = numericEntityRecognizerOpt.get.extractFrom(doc)
      setLabelsAndNorms(doc, numericMentions)
    }

    doc
  }

  private def assignPosTags(labels: Array[String], sent: Sentence): Unit = {
    assert(labels.length == sent.words.length)
    sent.tags = Some(postprocessPartOfSpeechTags(sent.words, labels))
  }

  /** Must be called after assignPosTags and lemmatize because it requires Sentence.tags and Sentence.lemmas */
  private def assignNamedEntityLabels(labels: Array[String], sent: Sentence): Unit = {
    assert(labels.length == sent.words.length)
    sent.entities = Some(labels)

    // NER labels from the custom NER
    val optionalNERLabels: Option[Array[String]] = optionalNER.map { ner =>
      val sentence = Sentence(
        sent.words,
        sent.startOffsets,
        sent.endOffsets,
        sent.words,
        sent.tags,
        sent.lemmas,
        entities = None,
        norms = None,
        chunks = None,
        tree = None,
        deps = EMPTY_GRAPH,
        relations = None
      )

      ner.find(sentence)
    }

    if(optionalNERLabels.isEmpty) {
      sent.entities = Some(NamedEntity.patch(labels))
    } else {
      sent.entities = Some(NamedEntity.patch(mergeNerLabels(labels, optionalNERLabels.get)))
    }
  }

  private def mergeNerLabels(generic: Array[String], custom: Array[String]): Array[String] = {
    require(generic.length == custom.length)

    val customNamedEntities = NamedEntity.collect(custom)
    val result = generic.toArray // A copy of the generic labels is created here.

    if (customNamedEntities.isEmpty)
      result
    else {
      val genericNamedEntities = NamedEntity.collect(generic)

      // The custom labels override the generic ones!
      NamedEntity.combine(result, genericNamedEntities, customNamedEntities)
    }
  }

  private def assignChunkLabels(labels: Array[String], sent: Sentence): Unit = {
    assert(labels.length == sent.words.length)
    sent.chunks = Some(labels)
  }

  private def assignDependencyLabels(headLabels: Array[String], labelLabels: Array[String], sent: Sentence): Unit = {
    val heads = convertToAbsoluteHeads(headLabels.map(_.toInt))
    val headsWithLabels = heads.zip(labelLabels).toArray
    parserPostProcessing(sent, headsWithLabels)

    val edges = new ListBuffer[Edge[String]]()
    val roots = new HashSet[Int]()
    for(i <- headsWithLabels.indices) {
      if(headsWithLabels(i)._1 != -1) {
        val edge = Edge[String](headsWithLabels(i)._1, i, headsWithLabels(i)._2)
        edges.append(edge)
      } else {
        roots += i
      }
    }
    
    val depGraph = new DirectedGraph[String](edges.toList, Some(sent.size), Some(roots.toSet))
    sent.graphs += GraphMap.UNIVERSAL_BASIC -> depGraph

    val enhancedDepGraph = ToEnhancedDependencies.generateUniversalEnhancedDependencies(sent, depGraph)
    sent.graphs += GraphMap.UNIVERSAL_ENHANCED -> enhancedDepGraph

  }

  private def convertToAbsoluteHeads(relativeHeads: IndexedSeq[Int]): IndexedSeq[Int] = {
    val heads = new ArrayBuffer[Int]()
    for(i <- relativeHeads.indices) {
      if(relativeHeads(i) == 0) {
        heads += -1
      } else {
        heads += i + relativeHeads(i)
      }
    }
    heads
  }
}

object BalaurProcessor {
  val logger:Logger = LoggerFactory.getLogger(classOf[CluProcessor])
  val prefix:String = "BalaurProcessor"

  val OUTSIDE = "O"
  val EMPTY_GRAPH = GraphMap()

  val NER_TASK = "NER"
  val POS_TASK = "POS"
  val CHUNKING_TASK = "Chunking"
  val DEPS_HEAD_TASK = "Deps Head"
  val DEPS_LABEL_TASK = "Deps Label"

  // maps a task name to a head index in the encoder
  val TASK_TO_INDEX = Map(
    NER_TASK -> 0,
    POS_TASK -> 1,
    CHUNKING_TASK -> 2,
    DEPS_HEAD_TASK -> 3,
    DEPS_LABEL_TASK -> 4
  )

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

  def newNumericEntityRecognizerOpt(seasonPathOpt: Option[String]): Option[NumericEntityRecognizer] = {
    seasonPathOpt.map(NumericEntityRecognizer(_))
  }
}