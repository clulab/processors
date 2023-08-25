package org.clulab.processors.clu

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.clulab.numeric.{NumericEntityRecognizer, setLabelsAndNorms}
import org.clulab.processors.{Document, IntermediateDocumentAttachment, Processor, Sentence}
import org.clulab.processors.clu.tokenizer._
import org.clulab.scala.WrappedArray._
import org.clulab.scala_transformers.encoder.TokenClassifier
import org.clulab.sequences.{LexiconNER, NamedEntity}
import org.clulab.struct.DirectedGraph
import org.clulab.struct.Edge
import org.clulab.struct.GraphMap
import org.clulab.utils.{BeforeAndAfter, Configured, DependencyUtils, Lazy, ScienceUtils, ToEnhancedDependencies, ToEnhancedSemanticRoles, MathUtils}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer

import BalaurProcessor._
import PostProcessor._
import scala.collection.mutable.ArrayBuffer

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
      val allLabelsAndScores = tokenClassifier.predictWithScores(sent.words)
      assignPosTags(allLabelsAndScores(TASK_TO_INDEX(POS_TASK)), sent)
      assignNamedEntityLabels(allLabelsAndScores(TASK_TO_INDEX(NER_TASK)), sent)
      assignChunkLabels(allLabelsAndScores(TASK_TO_INDEX(CHUNKING_TASK)), sent)
      assignDependencyLabels(
        allLabelsAndScores(TASK_TO_INDEX(DEPS_HEAD_TASK)), 
        allLabelsAndScores(TASK_TO_INDEX(DEPS_LABEL_TASK)), 
        sent)
    }

    // numeric entities using our numeric entity recognizer based on Odin rules
    if(numericEntityRecognizerOpt.nonEmpty) {
      val numericMentions = numericEntityRecognizerOpt.get.extractFrom(doc)
      setLabelsAndNorms(doc, numericMentions)
    }

    doc
  }

  private def assignPosTags(labels: Array[Array[(String, Float)]], sent: Sentence): Unit = {
    assert(labels.length == sent.words.length)
    sent.tags = Some(postprocessPartOfSpeechTags(sent.words, labels.map(_.head._1)))
  }

  /** Must be called after assignPosTags and lemmatize because it requires Sentence.tags and Sentence.lemmas */
  private def assignNamedEntityLabels(labels: Array[Array[(String, Float)]], sent: Sentence): Unit = {
    assert(labels.length == sent.words.length)

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

    val genericLabels = NamedEntity.patch(labels.map(_.head._1))

    if(optionalNERLabels.isEmpty) {
      sent.entities = Some(genericLabels)
    } else {
      //println(s"MERGING NE labels for sentence: ${sent.words.mkString(" ")}")
      //println(s"Generic labels: ${NamedEntity.patch(labels).mkString(", ")}")
      //println(s"Optional labels: ${optionalNERLabels.get.mkString(", ")}")
      val mergedLabels = NamedEntity.patch(mergeNerLabels(genericLabels, optionalNERLabels.get))
      //println(s"Merged labels: ${mergedLabels.mkString(", ")}")
      sent.entities = Some(mergedLabels)
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

      //println(s"Generic NamedEntity: ${genericNamedEntities.mkString(", ")}")
      //println(s"Custom NamedEntity: ${customNamedEntities.mkString(", ")}")

      // The custom labels override the generic ones!
      NamedEntity.combine(result, genericNamedEntities, customNamedEntities)
    }
  }

  private def assignChunkLabels(labels: Array[Array[(String, Float)]], sent: Sentence): Unit = {
    assert(labels.length == sent.words.length)
    sent.chunks = Some(labels.map(_.head._1))
  }

  private def interpolateHeadsAndLabels(
    headLabels: Array[Array[(String, Float)]], 
    labelLabels: Array[Array[(String, Float)]], 
    lambda: Float): Array[Array[(Int, String, Float)]] = {
      assert(headLabels.length == labelLabels.length)
      val allDepsPerModifier = new ArrayBuffer[Array[(Int, String, Float)]]
      for(i <- headLabels.indices) {
        assert(headLabels(i).length == labelLabels(i).length)
        val depCands = new ArrayBuffer[(Int, String, Float)]

        // convert logits to probabilities so we can interpolate them later
        val headProbs = MathUtils.softmaxFloat(headLabels(i).map(_._2))
        val labelProbs = MathUtils.softmaxFloat(labelLabels(i).map(_._2))

        for(j <- headLabels(i).indices) {
          val relHead = headLabels(i)(j)._1.toInt
          val label = labelLabels(i)(j)._1
          val interpolatedScore = lambda * headProbs(j) + (1.0 - lambda) * labelProbs(j) // these are probabilities not logits!
          depCands += Tuple3(relHead, label, interpolatedScore.toFloat)
        }
        allDepsPerModifier += depCands.toArray
      }
      allDepsPerModifier.toArray
    }

  private def assignDependencyLabels(
    headLabels: Array[Array[(String, Float)]], 
    labelLabels: Array[Array[(String, Float)]], 
    sent: Sentence): Unit = {

    val eisner = new EisnerEnsembleParser()  

    // preparate the input dependency table
    val headsAndLabels = interpolateHeadsAndLabels(headLabels, labelLabels, PARSING_INTERPOLATION_LAMBDA)
    val startingDeps = eisner.toDependencyTable(headsAndLabels, PARSING_TOPK)

    // the actual Eisner parsing algorithm
    val top = eisner.parse(startingDeps)

    // convert back to relative (or absolute) heads
    val bestDeps = eisner.generateOutput(top, headsAndLabels, startingDeps, false).toArray 
    parserPostProcessing(sent, bestDeps)

    //println("Sentence: " + sent.words.mkString(", "))
    //println("bestDeps: " + bestDeps.mkString(", "))

    // construct the dependency graphs to be stored in the sentence object
    val edges = new ListBuffer[Edge[String]]()
    val roots = new HashSet[Int]()
    for(i <- bestDeps.indices) {
      if(bestDeps(i)._1 != -1) {
        val edge = Edge[String](bestDeps(i)._1, i, bestDeps(i)._2)
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
    val absoluteHeads = relativeHeads.zipWithIndex.map { case (relativeHead, index) =>
      if (relativeHead == 0) -1
      else index + relativeHead
    }

    absoluteHeads
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

  val PARSING_INTERPOLATION_LAMBDA = 0.6f
  val PARSING_TOPK = 5

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