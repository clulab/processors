package org.clulab.processors.clu

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.clulab.numeric.NumericEntityRecognizer
import org.clulab.numeric.NumericUtils
import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.processors.clu.tokenizer.Lemmatizer
import org.clulab.processors.clu.tokenizer.{EnglishLemmatizer, PortugueseLemmatizer, SpanishLemmatizer}
import org.clulab.processors.clu.tokenizer.Tokenizer
import org.clulab.processors.clu.tokenizer.{OpenDomainEnglishTokenizer, OpenDomainPortugueseTokenizer, OpenDomainSpanishTokenizer}
import org.clulab.processors.hexatagging.HexaDecoder
import org.clulab.scala_transformers.encoder.EncoderMaxTokensRuntimeException
import org.clulab.scala_transformers.encoder.TokenClassifier
import org.clulab.sequences.{LexiconNER, NamedEntity}
import org.clulab.struct.DirectedGraph
import org.clulab.struct.GraphMap
import org.clulab.utils.{Configured, MathUtils, ToEnhancedDependencies}
import org.clulab.utils.WrappedArraySeq
import org.slf4j.{Logger, LoggerFactory}

import BalaurProcessor._

class BalaurProcessor protected (
  val config: Config,
  val lexiconNerOpt: Option[LexiconNER],
  val numericEntityRecognizerOpt: Option[NumericEntityRecognizer],
  wordTokenizer: Tokenizer,
  wordLemmatizer: Lemmatizer,
  tokenClassifier: TokenClassifier // multi-task classifier for all tasks addressed
) extends Processor with Configured {
  // This comes from scala-transformers, so we can't make a class from it here.
  private type PredictionScore = (String, Float)

  // standard, abbreviated constructor
  def this(
    config: Config = ConfigFactory.load("balaurprocessor"),
    lexiconNerOpt: Option[LexiconNER] = None,
    useNumericEntityRecognizer: Boolean = true,
    seasonPathOpt: Option[String] = Some("/org/clulab/numeric/SEASON.tsv")
  ) = this(
    config,
    lexiconNerOpt,
    if(useNumericEntityRecognizer) newNumericEntityRecognizerOpt(seasonPathOpt) else None,
    mkTokenizer(getConfigArgString(config, s"$prefix.language", Some("EN"))),
    mkLemmatizer(getConfigArgString(config, s"$prefix.language", Some("EN"))),
    // TokenClassifier.fromFiles(config.getString(s"$prefix.modelName"))
    TokenClassifier.fromResources(config.getString(s"$prefix.modelName"))
  )

  def copy(
    config: Config = config,
    lexiconNerOpt: Option[LexiconNER] = lexiconNerOpt,
    numericEntityRecognizerOpt: Option[NumericEntityRecognizer] = numericEntityRecognizerOpt,
    wordTokenizer: Tokenizer = wordTokenizer,
    wordLemmatizer: Lemmatizer = wordLemmatizer,
    tokenClassifier: TokenClassifier = tokenClassifier
  ): BalaurProcessor = {
    new BalaurProcessor(
      config,
      lexiconNerOpt,
      numericEntityRecognizerOpt,
      wordTokenizer,
      wordLemmatizer,
      tokenClassifier
    )
  }

  override def getConf: Config = config

  def tokenizer: Tokenizer = wordTokenizer

  override def mkDocument(text: String, keepText: Boolean): Document = { 
    DocumentMaker.mkDocument(tokenizer, text, keepText)
  }

  override def mkDocumentFromSentences(
    sentences: Iterable[String],
    keepText: Boolean, 
    charactersBetweenSentences: Int
  ): Document = {
    DocumentMaker.mkDocumentFromSentences(tokenizer, sentences, keepText, charactersBetweenSentences)
  }

  override def mkDocumentFromTokens(
    sentences: Iterable[Iterable[String]],
    keepText: Boolean, 
    charactersBetweenSentences: Int, 
    charactersBetweenTokens: Int
  ): Document = {
    DocumentMaker.mkDocumentFromTokens(sentences, keepText, charactersBetweenSentences, charactersBetweenSentences)
  }

  override def lemmatize(words: Seq[String]): Seq[String] = {
    val lemmas = words.zipWithIndex.map { case (word, index) =>
      val lemma = wordLemmatizer.lemmatizeWord(word)
      // A lemma may be empty in some weird Unicode situations.
      val nonEmptyLemma =
          if (lemma.isEmpty) {
            logger.debug(s"""WARNING: Found empty lemma for word #$index "$word" in sentence: ${words.mkString(" ")}""")
            word.toLowerCase()
          }
          else lemma

      nonEmptyLemma
    }

    lemmas
  }

  /** Generates cheap lemmas with the word in lower case, for languages where a lemmatizer is not available */
  def cheapLemmatize(sentence: Sentence): Seq[String] =
      sentence.words.map(_.toLowerCase())

  // TODO: Just don't include anything that calls this.
  def throwCannotCallException(methodName: String): Unit =
      throw new RuntimeException(s"ERROR: cannot call $methodName on its own in this processor!")

  override def tagPartsOfSpeech(doc: Document): Unit = throwCannotCallException("tagPartsOfSpeech")

  override def recognizeNamedEntities(doc: Document): Unit = throwCannotCallException("recognizeNamedEntities")

  override def parse(doc: Document): Unit = throwCannotCallException("parse")

  override def chunking(doc: Document): Unit = throwCannotCallException("chunking")

  private def throwNotSupportedException(methodName: String): Unit =
      throw new RuntimeException(s"ERROR: $methodName functionality not supported in this procecessor!")

  override def srl(doc: Document): Unit = throwNotSupportedException("srl")

  override def resolveCoreference(doc: Document): Unit = throwNotSupportedException("resolveCoreference")

  override def discourse(doc: Document): Unit = throwNotSupportedException("discourse")

  override def relationExtraction(doc: Document): Unit = throwNotSupportedException("relationExtraction")

  /**
   * Converts the MTL predictions into Sentence fields, in a new sentence
   * Inherit BalaurProcessor and redefine this method if you use a non-standard MTL model
   */
  def assignSentenceAnnotations(sentence: Sentence,
                                lemmas: Seq[String],
                                allLabelsAndScores: Array[Array[Array[(String, Float)]]]): Sentence = {
    val words = sentence.words
    val tags = mkPosTags(words, allLabelsAndScores(TASK_TO_INDEX(POS_TASK)))
    val entities = {
      val optionalEntities = mkNerLabelsOpt(words, sentence.startOffsets, sentence.endOffsets, tags, lemmas)

      mkNamedEntityLabels(words, allLabelsAndScores(TASK_TO_INDEX(NER_TASK)), optionalEntities)
    }
    val chunks = mkChunkLabels(words, allLabelsAndScores(TASK_TO_INDEX(CHUNKING_TASK)))
    val graphs = mkDependencyLabelsUsingHexaTags(
      words, lemmas, tags,
      allLabelsAndScores(TASK_TO_INDEX(HEXA_TERM_TASK)),
      allLabelsAndScores(TASK_TO_INDEX(HEXA_NONTERM_TASK))
    )

    // Entities and norms need to still be patched and filled in, so this is only a partly annotated sentence.
    val partlyAnnotatedSentence = sentence.copy(
      tags = Some(tags), lemmas = Some(lemmas), entities = Some(entities), chunks = Some(chunks), graphs = graphs
    )

    partlyAnnotatedSentence
  }

  /**
   * Implements domain-specific corrections to Sentence annotations (e.g., in Reach or reach-lite)
   * Inherit BalaurProcessor and redefine this method if you need to implement custom adjustments
   * @param sentence Input sentence
   */
  def correctAnnotations(sentence: Sentence): Sentence = {
    // empty in the open-domain BalaurProcessor
    sentence
  }

  override def annotate(doc: Document): Document = {
    // Process one sentence at a time through the MTL framework.
    val partlyAnnotatedSentences = doc.sentences.map { sentence =>
      val words = sentence.words
      // Lemmas are created deterministically, not through the MTL framework.
      val lemmas = lemmatize(words)

      try {
        val allLabelsAndScores = tokenClassifier.predictWithScores(words)
        assignSentenceAnnotations(sentence, lemmas, allLabelsAndScores)

      }
      // TODO: Improve error handling.
      catch {
        // No values, not even lemmas, will be included in the annotation is there was an exception.
        case e: EncoderMaxTokensRuntimeException =>
          // TODO: at some point do something smart here
          println(s"ERROR: This sentence exceeds the maximum number of tokens for the encoder and will not be annotated: ${sentence.words.mkString(" ")}")
          sentence
        case e: AssertionError =>
          println(s"ERROR: The output of predictWithScores does not satisfy assertions.  The sentence will not be annotated: ${sentence.words.mkString(" ")}")
          sentence
      }
    }

    val partlyAnnotatedDocument = doc.copy(sentences = partlyAnnotatedSentences)
    val fullyAnnotatedDocument = numericEntityRecognizerOpt.map { numericEntityRecognizer =>
      val numericMentions = numericEntityRecognizer.extractFrom(partlyAnnotatedDocument)
      val (newLabels, newNorms) = NumericUtils.mkLabelsAndNorms(partlyAnnotatedDocument, numericMentions)
      val fullyAnnotatedSentences = partlyAnnotatedDocument.sentences.indices.map { index =>
        partlyAnnotatedDocument.sentences(index).copy(
          entities = Some(newLabels(index)),
          norms = Some(newNorms(index))
        )
      }

      partlyAnnotatedDocument.copy(sentences = fullyAnnotatedSentences)
    }.getOrElse(partlyAnnotatedDocument)

    // custom annotation corrections
    // applied right at the end
    val correctedFullyAnnotatedSents = fullyAnnotatedDocument.sentences.map { sentence =>
      correctAnnotations(sentence)
    }

    val fullyAnnotatedDocumentWithCorrections =
      fullyAnnotatedDocument.copy(sentences = correctedFullyAnnotatedSents)
    fullyAnnotatedDocumentWithCorrections
  }

  private def mkPosTags(words: Seq[String], labels: Array[Array[(String, Float)]]): Seq[String] = {
    assert(labels.length == words.length)

    val rawTags = WrappedArraySeq(labels.map(_.head._1)).toImmutableSeq
    val cookedTags = PostProcessor.postprocessPartOfSpeechTags(words, rawTags)

    cookedTags
  }

  private def mkNerLabelsOpt(
    words: Seq[String], startOffsets: Seq[Int], endOffsets: Seq[Int],
    tags: Seq[String], lemmas: Seq[String]
  ): Option[Seq[String]] = {
    lexiconNerOpt.map { lexiconNer =>
      val sentence = Sentence(
        words, // TODO: Why isn't this raw?
        startOffsets,
        endOffsets,
        words,
        Some(tags),
        Some(lemmas)
      )

      lexiconNer.find(sentence)
    }
  }

  /** Must be called after assignPosTags and lemmatize because it requires Sentence.tags and Sentence.lemmas */
  private def mkNamedEntityLabels(words: Seq[String], labels: Array[Array[(String, Float)]], nerLabelsOpt: Option[Seq[String]]): Seq[String] = {
    assert(labels.length == words.length)

    val labelsSeq = WrappedArraySeq(labels.map(_.head._1)).toImmutableSeq
    val genericLabels = NamedEntity.patch(labelsSeq)
    val specificLabels = nerLabelsOpt.map { nerLabels =>
      //println(s"MERGING NE labels for sentence: ${sent.words.mkString(" ")}")
      //println(s"Generic labels: ${NamedEntity.patch(labels).mkString(", ")}")
      //println(s"Optional labels: ${optionalNERLabels.get.mkString(", ")}")
      val mergedLabels = mergeNerLabels(genericLabels, nerLabels)
      val patchedLabels = NamedEntity.patch(mergedLabels)
      //println(s"Merged labels: ${mergedLabels.mkString(", ")}")

      patchedLabels
    }.getOrElse(genericLabels)

    specificLabels
  }

  private def mergeNerLabels(generic: Seq[String], custom: Seq[String]): Seq[String] = {
    require(generic.length == custom.length)

    val customNamedEntities = NamedEntity.collect(custom)

    if (customNamedEntities.isEmpty)
      generic
    else {
      //println(s"Generic NamedEntity: ${genericNamedEntities.mkString(", ")}")
      //println(s"Custom NamedEntity: ${customNamedEntities.mkString(", ")}")
      val genericNamedEntities = NamedEntity.collect(generic)
      val combinedNamedEntities = NamedEntity.combine(generic, genericNamedEntities, customNamedEntities)

      combinedNamedEntities
    }
  }

  private def mkChunkLabels(words: Seq[String], labels: Array[Array[(String, Float)]]): Seq[String] = {
    assert(labels.length == words.length)

    WrappedArraySeq(labels.map(_.head._1)).toImmutableSeq
  }

  // TODO: This appears to be unused. But let's keep as an alternative scoring method
  // The head has one score, the label has another.  Here the two scores are interpolated
  // and the head and label are stored together in a single object with the score if the
  // object, the Dependency, has a valid absolute head.
  private def interpolateHeadsAndLabels(
    sentHeadPredictionScores: Array[Array[PredictionScore]],
    sentLabelPredictionScores: Array[Array[PredictionScore]],
    lambda: Float
  ): Array[Array[Dependency]] = {
    assert(sentHeadPredictionScores.length == sentLabelPredictionScores.length)

    val sentDependencies = sentHeadPredictionScores.zip(sentLabelPredictionScores).zipWithIndex.map { case ((wordHeadPredictionScores, wordLabelPredictionScores), wordIndex) =>
      assert(wordHeadPredictionScores.length == wordLabelPredictionScores.length)

      // convert logits to probabilities so we can interpolate them later
      val wordHeadProbabilities = MathUtils.softmaxFloat(wordHeadPredictionScores.map(_._2))
      val wordLabelProbabilities = MathUtils.softmaxFloat(wordLabelPredictionScores.map(_._2))
      val wordDependencies = wordHeadPredictionScores.indices.toArray.flatMap { predictionIndex =>
          // These are probabilities not logits!  Zipping four arrays is excessive.  This may interpolate
          // unnecessarily because the newOpt may return None because of an invalid absolute head.
          val interpolatedScore = lambda * wordHeadProbabilities(predictionIndex) +
              (1.0f - lambda) * wordLabelProbabilities(predictionIndex)

          Dependency.newOpt(wordIndex, sentHeadPredictionScores.indices, wordHeadPredictionScores(predictionIndex)._1,
              wordLabelPredictionScores(predictionIndex)._1, interpolatedScore)
      }
      // Although the wordHeadPredictionScores are sorted, the wordLabelPredictionScores aren't.
      // (They can't both be.)  Therefore, they need to be resorted here.  Thankfully, only the
      // valid Dependencies remain.
      val sortedWordDependencies = wordDependencies.sortBy(-_.score)

      sortedWordDependencies.toArray
    }

    sentDependencies.toArray
  }

  private def mkDependencyLabelsUsingHexaTags(
    words: Seq[String], lemmas: Seq[String], tags: Seq[String],
    termTags: Array[Array[PredictionScore]],
    nonTermTags: Array[Array[PredictionScore]]
  ): GraphMap.Type = {
    val verbose = false
    val size = words.length
    // bht is used just for debugging purposes here
    val (bht, deps, roots) = hexaDecoder.decode(termTags, nonTermTags, topK = 25, verbose)

    if (verbose && bht.nonEmpty) {
      println(bht)
      println(s"Dependencies (${deps.get.size}):")
      println(deps.mkString("\n"))
      println("Roots: " + roots.get.mkString(", "))
    }
    if (deps.nonEmpty && roots.nonEmpty) {
      // basic dependencies that replicate treebank annotations
      val depGraph = new DirectedGraph[String](deps.get, Some(size), roots)
      // enhanced dependencies as defined by Manning
      val enhancedDepGraph = ToEnhancedDependencies.generateUniversalEnhancedDependencies(words, lemmas, tags, depGraph)

      Map(
        GraphMap.UNIVERSAL_BASIC -> depGraph,
        GraphMap.UNIVERSAL_ENHANCED -> enhancedDepGraph,
        // ideally, hybrid dependencies should contain both syntactic dependencies and semantic roles
        // however, this processor produces only syntactic dependencies
        GraphMap.HYBRID_DEPENDENCIES -> enhancedDepGraph
      )
    }
    else
      GraphMap.empty
  }
}

object BalaurProcessor {
  val logger: Logger = LoggerFactory.getLogger(classOf[BalaurProcessor])
  val prefix: String = "BalaurProcessor"

  protected val NER_TASK = "NER"
  protected val POS_TASK = "POS"
  protected val CHUNKING_TASK = "Chunking"
  protected val HEXA_TERM_TASK = "Hexa Term"
  protected val HEXA_NONTERM_TASK = "Hexa NonTerm"

  // maps a task name to a head index in the encoder
  protected val TASK_TO_INDEX: Map[String, Int] = Seq(
    NER_TASK,
    POS_TASK,
    CHUNKING_TASK,
    HEXA_TERM_TASK,
    HEXA_NONTERM_TASK
  ).zipWithIndex.toMap

  private def mkTokenizer(lang: String): Tokenizer = lang match {
    case "PT" => new OpenDomainPortugueseTokenizer
    case "ES" => new OpenDomainSpanishTokenizer
    case "EN" | _ => new OpenDomainEnglishTokenizer
  }

  private def mkLemmatizer(lang: String): Lemmatizer = lang match {
    case "PT" => new PortugueseLemmatizer
    case "ES" => new SpanishLemmatizer
    case "EN" | _ => new EnglishLemmatizer
  }

  private def getConfigArgString (config: Config, argPath: String, defaultValue: Option[String]): String =
      if (config.hasPath(argPath)) config.getString(argPath)
      else if (defaultValue.nonEmpty) defaultValue.get
      else throw new RuntimeException(s"ERROR: parameter $argPath must be defined!")

  private def newNumericEntityRecognizerOpt(seasonPathOpt: Option[String]): Option[NumericEntityRecognizer] =
      seasonPathOpt.map(NumericEntityRecognizer(_))

  /** Converts hexa tags into dependencies */
  protected val hexaDecoder = new HexaDecoder()
}
