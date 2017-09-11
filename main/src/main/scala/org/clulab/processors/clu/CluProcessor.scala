package org.clulab.processors.clu

import edu.knowitall.tool.stem.MorphaStemmer
import org.clulab.processors.clu.sequences.PartOfSpeechTagger
import org.clulab.processors.clu.syntax._
import org.clulab.processors.clu.tokenizer.{OpenDomainEnglishTokenizer, Tokenizer}
import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.struct.GraphMap
import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.processors.clu.bio.BioPreProcessor
import org.clulab.utils.Configured
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Processor that uses only tools that are under Apache License
  * Currently supports tokenization (in-house),
  *   lemmatization (Morpha),
  *   POS tagging (in-house MEMM),
  *   dependency parsing (ensemble of Malt models)
  */
class CluProcessor (val config: Config) extends Processor with Configured {

  override def getConf: Config = config

  // should we intern strings or not?
  val internStrings:Boolean = getArgBoolean("CluProcessor.internStrings", Some(false))

  // this class preprocesses the text before any computation happens
  lazy val preProcessor:Option[PreProcessor] =
    getArgString("CluProcessor.pre.type", Some("none")) match {
      case "bio" => Some(new BioPreProcessor(
        removeFigTabReferences = getArgBoolean("CluProcessor.pre.removeFigTabReferences", Some(true)),
        removeBibReferences = getArgBoolean("CluProcessor.pre.removeBibReferences", Some(true))))
      case "none" => None
      case _ => throw new RuntimeException("ERROR: Unknown argument value for CluProcessor.pre.type!")
    }

  lazy val tokenizer: Tokenizer =
    new OpenDomainEnglishTokenizer

  lazy val posTagger: PartOfSpeechTagger =
    PartOfSpeechTagger.loadFromResource(getArgString("CluProcessor.posModel", None))

  lazy val depParser: Parser =
    //new MaltWrapper(getArgString("CluProcessor.parseModel", None), internStrings)
    new EnsembleMaltParser(getArgStrings("CluProcessor.parseModels", None))

  override def annotate(doc:Document): Document = {
    // with this processor, we lemmatize first, because this POS tagger uses lemmas as features
    lemmatize(doc)
    tagPartsOfSpeech(doc)
    recognizeNamedEntities(doc)
    parse(doc)
    chunking(doc)
    resolveCoreference(doc)
    discourse(doc)
    doc.clear()
    doc
  }

  override def preprocessText(origText:String):String = {
    if(preProcessor.nonEmpty) preProcessor.get.process(origText)
    else origText
  }

  /** Constructs a document of tokens from free text; includes sentence splitting and tokenization */
  def mkDocument(text:String, keepText:Boolean = false): Document = {
    val sents = tokenizer.tokenize(text)
    val doc = new Document(sents)
    if(keepText) doc.text = Some(text)
    doc
  }

  /** Constructs a document of tokens from an array of untokenized sentences */
  def mkDocumentFromSentences(sentences:Iterable[String],
                              keepText:Boolean = false,
                              charactersBetweenSentences:Int = 1): Document = {
    val sents = new ArrayBuffer[Sentence]()
    for(text <- sentences) {
      sents ++= tokenizer.tokenize(text, sentenceSplit = false) // we produce a single sentence here!
    }
    val doc = new Document(sents.toArray)
    if(keepText) doc.text = Some(sentences.mkString(mkSep(charactersBetweenSentences)))
    doc
  }

  /** Constructs a document of tokens from an array of tokenized sentences */
  def mkDocumentFromTokens(sentences:Iterable[Iterable[String]],
                           keepText:Boolean = false,
                           charactersBetweenSentences:Int = 1,
                           charactersBetweenTokens:Int = 1): Document = {
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
      sents += new Sentence(sentence.toArray, startOffsets.toArray, endOffsets.toArray)
      charOffset += charactersBetweenSentences
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
    for (i <- 0 until size) os.append(" ")
    os.toString()
  }

  private def in(s:String):String = {
    if (internStrings) Processor.internString(s)
    else s
  }

  /** Part of speech tagging */
  def tagPartsOfSpeech(doc:Document) {
    basicSanityCheck(doc)
    for(sent <- doc.sentences) {
      val tags = posTagger.classesOf(sent)
      sent.tags = Some(tags)
    }
  }

  /** Lematization; modifies the document in place */
  def lemmatize(doc:Document) {
    basicSanityCheck(doc)
    for(sent <- doc.sentences) {
      val lemmas = new Array[String](sent.size)
      for(i <- sent.words.indices) {
        var lemma = MorphaStemmer.lemmatize(sent.words(i))
        // for some strings, MorphaStemmer returns empty strings
        if(lemma.isEmpty)
          lemma = sent.words(i).toLowerCase()
        lemmas(i) = lemma
      }
      sent.lemmas = Some(lemmas)
    }
  }

  /** NER; modifies the document in place */
  def recognizeNamedEntities(doc:Document) {
    // TODO
  }

  /** Syntactic parsing; modifies the document in place */
  def parse(doc:Document) {
    basicSanityCheck(doc)
    if (doc.sentences.head.tags.isEmpty)
      throw new RuntimeException("ERROR: you have to run the POS tagger before parsing!")
    if (doc.sentences.head.lemmas.isEmpty)
      throw new RuntimeException("ERROR: you have to run the lemmatizer before parsing!")

    for (sentence <- doc.sentences) {
      val dg = depParser.parseSentence(sentence)
      sentence.setDependencies(GraphMap.STANFORD_BASIC, dg)
      sentence.setDependencies(GraphMap.STANFORD_COLLAPSED,
        EnhancedDependencies.generateEnhancedDependencies(sentence, dg))
    }
  }
  
  /** Shallow parsing; modifies the document in place */
  def chunking(doc:Document) {
    // TODO
  }

  /** Coreference resolution; modifies the document in place */
  def resolveCoreference(doc:Document) { }

  /** Discourse parsing; modifies the document in place */
  def discourse(doc:Document) { }

  private def basicSanityCheck(doc:Document): Unit = {
    if (doc.sentences == null)
      throw new RuntimeException("ERROR: Document.sentences == null!")
    if (doc.sentences.length != 0 && doc.sentences(0).words == null)
      throw new RuntimeException("ERROR: Sentence.words == null!")
  }
  
}

trait PreProcessor {
  def process(text:String):String
}

object CluProcessor {
  val logger:Logger = LoggerFactory.getLogger(classOf[CluProcessor])

  def mkOpenCluProcessor():CluProcessor = {
    val config = ConfigFactory.load("cluprocessoropen")
    new CluProcessor(config)
  }

  def mkBioCluProcessor():CluProcessor = {
    val config = ConfigFactory.load("cluprocessorbio")
    new CluProcessor(config)
  }
}