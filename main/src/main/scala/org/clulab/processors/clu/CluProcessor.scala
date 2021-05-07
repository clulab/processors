package org.clulab.processors.clu

import org.clulab.processors.clu.tokenizer._
import org.clulab.processors.{Document, IntermediateDocumentAttachment, Processor, Sentence}
import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.utils.{Configured, DependencyUtils, ScienceUtils, ToEnhancedDependencies, ToEnhancedSemanticRoles}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import CluProcessor._
import org.clulab.dynet.{AnnotatedSentence, ConstEmbeddingParameters, ConstEmbeddingsGlove, Metal}
import org.clulab.struct.{DirectedGraph, Edge, GraphMap}
import org.clulab.utils.Timers

import scala.collection.immutable.HashMap

/**
  * Processor that uses only tools that are under Apache License
  * Currently supports:
  *   tokenization (in-house),
  *   lemmatization (Morpha, copied in our repo to minimize dependencies),
  *   POS tagging, NER, chunking, dependency parsing - using our MTL architecture (dep parsing coming soon)
  */
class CluProcessor (val config: Config = ConfigFactory.load("cluprocessor")) extends Processor with Configured {

  override def getConf: Config = config

  // should we intern strings or not?
  val internStrings:Boolean = getArgBoolean(s"$prefix.internStrings", Some(false))

  // This strange construction is designed to allow subclasses access to the value of the tokenizer while
  // at the same time allowing them to override the value.
  // val tokenizer: Tokenizer = new ModifiedTokenizer(super.tokenizer)
  // does not work in a subclass because super.tokenizer is invalid.  Instead it needs to be something like
  // val tokenizer: Tokenizer = new ModifiedTokenizer(localTokenizer)
  protected lazy val localTokenizer: Tokenizer = getArgString(s"$prefix.language", Some("EN")) match {
    case "PT" => new OpenDomainPortugueseTokenizer
    case "ES" => new OpenDomainSpanishTokenizer
    case _ => new OpenDomainEnglishTokenizer
  }

  // the actual tokenizer
  lazy val tokenizer: Tokenizer = localTokenizer

  // the lemmatizer
  lazy val lemmatizer: Lemmatizer = getArgString(s"$prefix.language", Some("EN")) match {
    case "PT" => new PortugueseLemmatizer
    case "ES" => new SpanishLemmatizer
    case _ => new EnglishLemmatizer
  }

  // one of the multi-task learning (MTL) models, which covers: POS, chunking, and SRL (predicates)
  lazy val mtlPosChunkSrlp: Metal = getArgString(s"$prefix.language", Some("EN")) match {
    case "PT" => throw new RuntimeException("PT model not trained yet") // Add PT
    case "ES" => throw new RuntimeException("ES model not trained yet") // Add ES
    case _ => Metal(getArgString(s"$prefix.mtl-pos-chunk-srlp", Some("mtl-en-pos-chunk-srlp")))
  }

  // one of the multi-task learning (MTL) models, which covers: NER
  lazy val mtlNer: Metal = getArgString(s"$prefix.language", Some("EN")) match {
    case "PT" => throw new RuntimeException("PT model not trained yet") // Add PT
    case "ES" => throw new RuntimeException("ES model not trained yet") // Add ES
    case _ => Metal(getArgString(s"$prefix.mtl-ner", Some("mtl-en-ner")))
  }

  // one of the multi-task learning (MTL) models, which covers: SRL (arguments)
  lazy val mtlSrla: Metal = getArgString(s"$prefix.language", Some("EN")) match {
    case "PT" => throw new RuntimeException("PT model not trained yet") // Add PT
    case "ES" => throw new RuntimeException("ES model not trained yet") // Add ES
    case _ => Metal(getArgString(s"$prefix.mtl-srla", Some("mtl-en-srla")))
  }

  lazy val mtlDepsHead: Metal = getArgString(s"$prefix.language", Some("EN")) match {
    case "PT" => throw new RuntimeException("PT model not trained yet") // Add PT
    case "ES" => throw new RuntimeException("ES model not trained yet") // Add ES
    case _ => Metal(getArgString(s"$prefix.mtl-depsh", Some("mtl-en-depsh")))
  }

  lazy val mtlDepsLabel: Metal = getArgString(s"$prefix.language", Some("EN")) match {
    case "PT" => throw new RuntimeException("PT model not trained yet") // Add PT
    case "ES" => throw new RuntimeException("ES model not trained yet") // Add ES
    case _ => Metal(getArgString(s"$prefix.mtl-depsl", Some("mtl-en-depsl")))
  }

  case class EmbeddingsAttachment (embeddings: ConstEmbeddingParameters)
    extends IntermediateDocumentAttachment

  def mkConstEmbeddings(doc: Document): Unit = {
    // fetch the const embeddings from GloVe. All our models need them
    val embeddings = ConstEmbeddingsGlove.mkConstLookupParams(doc)
    // now set them as an attachment, so they are available to all downstream methods wo/ changing the API
    doc.addAttachment(CONST_EMBEDDINGS_ATTACHMENT_NAME, EmbeddingsAttachment(embeddings))
  }

  override def annotate(doc:Document): Document = {
    // we need this before using the DyNet models
    Timers.getOrNew("mkConstEmbeddings").time {
      mkConstEmbeddings(doc)
    }

    Timers.getOrNew("tagPartsOfSpeech").time {
      tagPartsOfSpeech(doc) // the call to the POS/chunking/SRLp MTL is in here
    }
    //println("After POS")
    //println(doc.sentences.head.tags.get.mkString(", "))
    Timers.getOrNew("recognizedNamedEntities").time {
      recognizeNamedEntities(doc) // the call to the NER MTL is in here
    }
    //println("After NER")
    //println(doc.sentences.head.entities.get.mkString(", "))
    Timers.getOrNew("chunking").time {
      chunking(doc) // Nothing, kept for the record
    }
    Timers.getOrNew("parse").time {
      parse(doc) // dependency parsing
    }
    //println("After parsing")
    //println(doc.sentences.head.universalEnhancedDependencies.get)

    Timers.getOrNew("lemmatize").time {
      lemmatize(doc) // lemmatization has access to POS tags, which are needed in some languages
    }

    Timers.getOrNew("srl").time {
      srl(doc) // SRL (arguments)
    }
    //println("After SRL")
    //println(doc.sentences.head.semanticRoles.get)

    // these are not implemented yet
    Timers.getOrNew("resolveCoreference").time {
      resolveCoreference(doc)
    }
    Timers.getOrNew("discourse").time {
      discourse(doc)
    }

    Timers.getOrNew("clear").time {
      doc.clear()
    }
    Timers.getOrNew("removeAttachment").time {
      doc.removeAttachment(CONST_EMBEDDINGS_ATTACHMENT_NAME)
    }
    doc
  }

  /** Constructs a document of tokens from free text; includes sentence splitting and tokenization */
  def mkDocument(text:String, keepText:Boolean = false): Document = {
    CluProcessor.mkDocument(tokenizer, text, keepText)
  }

  /** Constructs a document of tokens from an array of untokenized sentences */
  def mkDocumentFromSentences(sentences:Iterable[String],
                              keepText:Boolean = false,
                              charactersBetweenSentences:Int = 1): Document = {
    CluProcessor.mkDocumentFromSentences(tokenizer, sentences, keepText, charactersBetweenSentences)
  }

  /** Constructs a document of tokens from an array of tokenized sentences */
  def mkDocumentFromTokens(sentences:Iterable[Iterable[String]],
                           keepText:Boolean = false,
                           charactersBetweenSentences:Int = 1,
                           charactersBetweenTokens:Int = 1): Document = {
    CluProcessor.mkDocumentFromTokens(tokenizer, sentences, keepText, charactersBetweenSentences, charactersBetweenTokens)
  }

  class PredicateAttachment(val predicates: IndexedSeq[IndexedSeq[Int]]) extends IntermediateDocumentAttachment

  /** Produces POS tags, chunks, and semantic role predicates for one sentence */
  def tagSentence(words: IndexedSeq[String], embeddings: ConstEmbeddingParameters):
    (IndexedSeq[String], IndexedSeq[String], IndexedSeq[String]) = {

    val allLabels = mtlPosChunkSrlp.predictJointly(AnnotatedSentence(words), embeddings)
    val tags = allLabels(0)
    val chunks = allLabels(1)
    val preds = allLabels(2)
    (tags, chunks, preds)
  }

  /** Produces NE labels for one sentence */
  def nerSentence(words: Array[String],
                  lemmas: Option[Array[String]],
                  tags: Array[String], // this are only used by the NumericEntityRecognizer
                  startCharOffsets: Array[Int],
                  endCharOffsets: Array[Int],
                  docDateOpt: Option[String],
                  embeddings: ConstEmbeddingParameters): (IndexedSeq[String], Option[IndexedSeq[String]]) = {
    val allLabels = mtlNer.predictJointly(AnnotatedSentence(words), embeddings)
    (allLabels(0), None)
  }

  /** Gets the index of all predicates in this sentence */
  def getPredicateIndexes(preds: IndexedSeq[String]): IndexedSeq[Int] = {
    val predsInSent = new ArrayBuffer[Int]()
    var done = false
    var offset = 0
    while(! done) {
      val idx = preds.indexOf("B-P", offset)

      if(idx >= 0) {
        predsInSent += idx
        offset = idx + 1
      } else {
        done = true
      }
    }

    predsInSent
  }

  /** Dependency parsing */
  def parseSentence(words: IndexedSeq[String],
                    posTags: IndexedSeq[String],
                    nerLabels: IndexedSeq[String],
                    embeddings: ConstEmbeddingParameters): DirectedGraph[String] = {

    //println(s"Words: ${words.mkString(", ")}")
    //println(s"Tags: ${posTags.mkString(", ")}")
    //println(s"NEs: ${nerLabels.mkString(", ")}")

    val annotatedSentence =
      AnnotatedSentence(words, Some(posTags), Some(nerLabels))

    /*
    val headsAsString = mtlDepsHead.predict(0, annotatedSentence)
    println("REL HEADS: " + headsAsString.mkString(", "))
    val heads = new ArrayBuffer[Int]()
    for(i <- headsAsString.indices) {
      val relativeHead = headsAsString(i).toInt
      if(relativeHead == 0) { // this is the root
        heads += -1
      } else {
        heads += i + relativeHead
      }
    }
    */

    val headsAsStringsWithScores = mtlDepsHead.predictWithScores(0, annotatedSentence, embeddings)
    val heads = new ArrayBuffer[Int]()
    for(wi <- headsAsStringsWithScores.indices) {
      val predictionsForThisWord = headsAsStringsWithScores(wi)

      // pick the prediction with the highest score, which makes sense for the current sentence
      var done = false
      for(hi <- predictionsForThisWord.indices if ! done) {
        try {
          val relativeHead = predictionsForThisWord(hi)._1.toInt
          if (relativeHead == 0) { // this is the root
            heads += -1
            done = true
          } else {
            val headPosition = wi + relativeHead
            if (headPosition >= 0 && headPosition < words.size) {
              heads += headPosition
              done = true
            }
          }
        } catch {
          // some valid predictions may not be integers, e.g., "<STOP>" may be predicted by the sequence model
          case e: NumberFormatException => done = false
        }
      }
      if(! done) {
        // we should not be here, but let's be safe
        // if nothing good was found, assume root
        heads += -1
      }
    }

    val annotatedSentenceWithHeads =
      AnnotatedSentence(words, Some(posTags), Some(nerLabels), Some(heads))

    val labels = mtlDepsLabel.predict(0, annotatedSentenceWithHeads, embeddings)
    assert(labels.size == heads.size)
    //println(s"Labels: ${labels.mkString(", ")}")

    val edges = new ListBuffer[Edge[String]]()
    val roots = new mutable.HashSet[Int]()

    for(i <- heads.indices) {
      if(heads(i) == -1) {
        roots += i
      } else {
        val edge = Edge[String](heads(i), i, labels(i))
        edges.append(edge)
      }
    }

    new DirectedGraph[String](edges.toList, roots.toSet)
  }

  def srlSentence(sent: Sentence,
                  predicateIndexes: IndexedSeq[Int],
                  embeddings: ConstEmbeddingParameters): DirectedGraph[String] = {
    srlSentence(sent.words, sent.tags.get, sent.entities.get, predicateIndexes, embeddings)
  }

  /** Produces semantic role frames for one sentence */
  def srlSentence(words: IndexedSeq[String],
                  posTags: IndexedSeq[String],
                  nerLabels: IndexedSeq[String],
                  predicateIndexes: IndexedSeq[Int],
                  embeddings: ConstEmbeddingParameters): DirectedGraph[String] = {
    val edges = new ListBuffer[Edge[String]]()
    val roots = new mutable.HashSet[Int]()

    // all predicates become roots
    roots ++= predicateIndexes

    for(pi <- predicateIndexes.indices) {
      // SRL needs POS tags and NEs, as well as the position of the predicate
      val headPositions = new ArrayBuffer[Int]()
      val pred = predicateIndexes(pi)
      for(i <- words.indices) {
        headPositions += pred
      }

      val annotatedSentence =
        AnnotatedSentence(words, Some(posTags), Some(nerLabels), Some(headPositions))

      val argLabels = mtlSrla.predict(0, annotatedSentence, embeddings)

      for(ai <- argLabels.indices) {
        if(argLabels(ai) != "O") {
          val edge = Edge[String](pred, ai, argLabels(ai))
          edges += edge
        }
      }
    }

    new DirectedGraph[String](edges.toList, roots.toSet, Some(words.length))
  }

  private def getEmbeddings(doc: Document): ConstEmbeddingParameters =
    doc.getAttachment(CONST_EMBEDDINGS_ATTACHMENT_NAME).get.asInstanceOf[EmbeddingsAttachment].embeddings

  /** Part of speech tagging + chunking + SRL (predicates), jointly */
  override def tagPartsOfSpeech(doc:Document) {
    basicSanityCheck(doc)

    val embeddings = getEmbeddings(doc)
    val predsForAllSents = new ArrayBuffer[IndexedSeq[Int]]()

    for(sent <- doc.sentences) {
      val (tags, chunks, preds) = tagSentence(sent.words, embeddings)
      sent.tags = Some(tags.toArray)
      sent.chunks = Some(chunks.toArray)
      predsForAllSents += getPredicateIndexes(preds)
    }

    // store the index of all predicates as a doc attachment
    doc.addAttachment(PREDICATE_ATTACHMENT_NAME, new PredicateAttachment(predsForAllSents))
  }

  /** Lematization; modifies the document in place */
  override def lemmatize(doc:Document) {
    basicSanityCheck(doc)
    for(sent <- doc.sentences) {
      //println(s"Lemmatize sentence: ${sent.words.mkString(", ")}")
      val lemmas = new Array[String](sent.size)
      for(i <- sent.words.indices) {
        lemmas(i) = lemmatizer.lemmatizeWord(sent.words(i))

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
  def cheapLemmatize(doc:Document) {
    basicSanityCheck(doc)
    for(sent <- doc.sentences) {
      val lemmas = sent.words.map(_.toLowerCase())
      sent.lemmas = Some(lemmas)
    }
  }

  /** NER; modifies the document in place */
  override def recognizeNamedEntities(doc:Document): Unit = {
    basicSanityCheck(doc)
    val embeddings = getEmbeddings(doc)
    val docDate = doc.getDCT
    for(sent <- doc.sentences) {
      val (labels, norms) = nerSentence(
        sent.words,
        sent.lemmas,
        sent.tags.get,
        sent.startOffsets,
        sent.endOffsets,
        docDate,
        embeddings)

      sent.entities = Some(labels.toArray)
      if(norms.nonEmpty) {
        sent.norms = Some(norms.get.toArray)
      }
    }
  }

  private def hasDep(dependencies: Array[(Int, String)], label: String): Boolean = {
    for(d <- dependencies) {
      if (d._2 == label) {
        return true
      }
    }

    false
  }

  private def predicateCorrections(origPreds: IndexedSeq[Int], sentence: Sentence): IndexedSeq[Int] = {

    if(sentence.universalBasicDependencies.isEmpty) return origPreds
    if(sentence.tags.isEmpty) return origPreds
    
    val preds = origPreds.toSet
    val newPreds = new mutable.HashSet[Int]()
    newPreds ++= preds

    val outgoing = sentence.universalBasicDependencies.get.outgoingEdges
    val words = sentence.words
    val tags = sentence.tags.get

    for(i <- words.indices) {
      if(! preds.contains(i)) {
        // -ing NN with a compound outgoing dependency
        if(words(i).endsWith("ing") && tags(i).startsWith("NN") &&
           outgoing.length > i && hasDep(outgoing(i), "compound")) {
          newPreds += i
        }
      }
    }

    newPreds.toVector.sorted
  }

  override def srl(doc: Document): Unit = {
    val predicatesAttachment = doc.getAttachment(PREDICATE_ATTACHMENT_NAME)
    assert(predicatesAttachment.nonEmpty)
    assert(doc.getAttachment(CONST_EMBEDDINGS_ATTACHMENT_NAME).isDefined)
    val embeddings = getEmbeddings(doc)

    if(doc.sentences.length > 0) {
      assert(doc.sentences(0).tags.nonEmpty)
      assert(doc.sentences(0).entities.nonEmpty)
      assert(doc.sentences(0).universalBasicDependencies.nonEmpty)
    }

    val predicates = predicatesAttachment.get.asInstanceOf[PredicateAttachment].predicates
    assert(predicates.length == doc.sentences.length)

    // If there are no predicates, a test sentence will not provoke the calling of srlSentence(), which
    // will prevent reference to the lazy val mtlSrla.  This means that the model will not be created
    // until later, possibly in a parallel processing situation which will result sooner or later and
    // under the right conditions in DyNet crashing.  Therefore, make preemptive reference to mtlSrla
    // here so that it is sure to be initialized, regardless of the priming sentence.
    assert(mtlSrla != null)

    // generate SRL frames for each predicate in each sentence
    for(si <- predicates.indices) {
      val sentence = doc.sentences(si)
      val predicateIndexes = 
      	predicateCorrections(predicates(si), sentence)
      val semanticRoles = srlSentence(sentence, predicateIndexes, embeddings)

      sentence.graphs += GraphMap.SEMANTIC_ROLES -> semanticRoles

      // enhanced semantic roles need basic universal dependencies to be generated
      if(sentence.graphs.contains(GraphMap.UNIVERSAL_BASIC)) {
        val enhancedRoles = ToEnhancedSemanticRoles.generateEnhancedSemanticRoles(
          sentence, sentence.universalBasicDependencies.get, semanticRoles)
        sentence.graphs += GraphMap.ENHANCED_SEMANTIC_ROLES -> enhancedRoles
      }

      // hybrid = universal enhanced + roles enhanced
      if(sentence.graphs.contains(GraphMap.UNIVERSAL_ENHANCED) &&
         sentence.graphs.contains(GraphMap.ENHANCED_SEMANTIC_ROLES)) {

        val mergedGraph = DependencyUtils.mergeGraphs(
          sentence.universalEnhancedDependencies.get,
          sentence.enhancedSemanticRoles.get)
        sentence.graphs += GraphMap.HYBRID_DEPENDENCIES -> mergedGraph
      }
    }

    doc.removeAttachment(PREDICATE_ATTACHMENT_NAME)
  }

  /** Syntactic parsing; modifies the document in place */
  def parse(doc:Document): Unit = {
    if(doc.sentences.length > 0) {
      assert(doc.sentences(0).tags.nonEmpty)
      assert(doc.sentences(0).entities.nonEmpty)
    }
    assert(doc.getAttachment(CONST_EMBEDDINGS_ATTACHMENT_NAME).isDefined)
    val embeddings = getEmbeddings(doc)

    for(sent <- doc.sentences) {
      val depGraph = parseSentence(sent.words, sent.tags.get, sent.entities.get, embeddings)
      sent.graphs += GraphMap.UNIVERSAL_BASIC -> depGraph

      val enhancedDepGraph = ToEnhancedDependencies.generateUniversalEnhancedDependencies(sent, depGraph)
      sent.graphs += GraphMap.UNIVERSAL_ENHANCED -> enhancedDepGraph
    }
  }

  /** Shallow parsing; modifies the document in place */
  def chunking(doc:Document): Unit = {
    // Nop, covered by MTL
  }

  /** Coreference resolution; modifies the document in place */
  def resolveCoreference(doc:Document) {
    // TODO. Implement me
  }

  /** Discourse parsing; modifies the document in place */
  def discourse(doc:Document) {
    // TODO. Implement me
  }

  /** Relation extraction; modifies the document in place. */
  override def relationExtraction(doc: Document): Unit = {
    // TODO. We will probably not include this.
  }

  def basicSanityCheck(doc:Document): Unit = {
    if (doc.sentences == null)
      throw new RuntimeException("ERROR: Document.sentences == null!")
    if (doc.sentences.length != 0 && doc.sentences(0).words == null)
      throw new RuntimeException("ERROR: Sentence.words == null!")
    if(doc.getAttachment(CluProcessor.CONST_EMBEDDINGS_ATTACHMENT_NAME).isEmpty)
      throw new RuntimeException("ERROR: Const embeddings not set!")
  }

}

trait SentencePostProcessor {
  def process(sentence: Sentence)
}

/** CluProcessor for Spanish */
class SpanishCluProcessor extends CluProcessor(config = ConfigFactory.load("cluprocessorspanish"))

/** CluProcessor for Portuguese */
class PortugueseCluProcessor extends CluProcessor(config = ConfigFactory.load("cluprocessorportuguese")) {

  val scienceUtils = new ScienceUtils

  /** Constructs a document of tokens from free text; includes sentence splitting and tokenization */
  override def mkDocument(text:String, keepText:Boolean = false): Document = {
    // FIXME by calling replaceUnicodeWithAscii we are normalizing unicode and keeping accented characters of interest,
    // but we are also replacing individual unicode characters with sequences of characters that can potentially be greater than one
    // which means we may lose alignment to the original text
    val textWithAccents = scienceUtils.replaceUnicodeWithAscii(text, keepAccents = true)
    CluProcessor.mkDocument(tokenizer, textWithAccents, keepText)
  }

  /** Lematization; modifies the document in place */
  override def lemmatize(doc:Document) {
    basicSanityCheck(doc)
    for(sent <- doc.sentences) {
      // check if sentence tags were defined
      // if not, generate cheap lemmas
      if (sent.tags.isDefined) {
        //println(s"Lemmatize sentence: ${sent.words.mkString(", ")}")
        val lemmas = new Array[String](sent.size)
        for (i <- sent.words.indices) {
          lemmas(i) = lemmatizer.lemmatizeWord(sent.words(i), Some(sent.tags.get(i)))

          // a lemma may be empty in some weird Unicode situations
          if(lemmas(i).isEmpty) {
            logger.debug(s"""WARNING: Found empty lemma for word #$i "${sent.words(i)}" in sentence: ${sent.words.mkString(" ")}""")
            lemmas(i) = sent.words(i).toLowerCase()
          }
        }
        sent.lemmas = Some(lemmas)
      } else {
        cheapLemmatize(doc)
      }
    }
  }

}

object CluProcessor {
  val logger:Logger = LoggerFactory.getLogger(classOf[CluProcessor])
  val prefix:String = "CluProcessor"

  val PREDICATE_ATTACHMENT_NAME = "predicates"

  val CONST_EMBEDDINGS_ATTACHMENT_NAME = "ce"

  /** Constructs a document of tokens from free text; includes sentence splitting and tokenization */
  def mkDocument(tokenizer:Tokenizer,
                 text:String,
                 keepText:Boolean): Document = {
    val sents = tokenizer.tokenize(text)
    val doc = new Document(sents)
    if(keepText) doc.text = Some(text)
    doc
  }

  /** Constructs a document of tokens from an array of untokenized sentences */
  def mkDocumentFromSentences(tokenizer:Tokenizer,
                              sentences:Iterable[String],
                              keepText:Boolean,
                              charactersBetweenSentences:Int): Document = {
    val sents = new ArrayBuffer[Sentence]()
    var characterOffset = 0
    for(text <- sentences) {
      val sent = tokenizer.tokenize(text, sentenceSplit = false).head // we produce a single sentence here!

      // update character offsets between sentences
      for(i <- 0 until sent.size) {
        sent.startOffsets(i) += characterOffset
        sent.endOffsets(i) += characterOffset
      }

      // move the character offset after the current sentence
      characterOffset = sent.endOffsets.last + charactersBetweenSentences

      //println("SENTENCE: " + sent.words.mkString(", "))
      //println("Start offsets: " + sent.startOffsets.mkString(", "))
      //println("End offsets: " + sent.endOffsets.mkString(", "))
      sents += sent
    }
    val doc = new Document(sents.toArray)
    if(keepText) doc.text = Some(sentences.mkString(mkSep(charactersBetweenSentences)))
    doc
  }

  /** Constructs a document of tokens from an array of tokenized sentences */
  def mkDocumentFromTokens(tokenizer:Tokenizer,
                           sentences:Iterable[Iterable[String]],
                           keepText:Boolean,
                           charactersBetweenSentences:Int,
                           charactersBetweenTokens:Int): Document = {
    var charOffset = 0
    var sents = new ArrayBuffer[Sentence]()
    val text = new StringBuilder
    for(sentence <- sentences) {
      val startOffsets = new ArrayBuffer[Int]()
      val endOffsets = new ArrayBuffer[Int]()
      for(word <- sentence) {
        startOffsets += charOffset
        charOffset += word.length
        endOffsets += charOffset
        charOffset += charactersBetweenTokens
      }
      // note: NO postprocessing happens in this case, so use it carefully!
      sents += new Sentence(sentence.toArray, startOffsets.toArray, endOffsets.toArray, sentence.toArray)
      charOffset += charactersBetweenSentences - charactersBetweenTokens
      if(keepText) {
        text.append(sentence.mkString(mkSep(charactersBetweenTokens)))
        text.append(mkSep(charactersBetweenSentences))
      }
    }

    val doc = new Document(sents.toArray)
    if(keepText) doc.text = Some(text.toString)
    doc
  }

  private def mkSep(size:Int):String = {
    val os = new mutable.StringBuilder
    for (_ <- 0 until size) os.append(" ")
    os.toString()
  }
}


