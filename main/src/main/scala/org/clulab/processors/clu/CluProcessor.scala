package org.clulab.processors.clu

import org.clulab.processors.clu.tokenizer._
import org.clulab.processors.{Document, Processor, Sentence}
import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.utils.Configured
import org.clulab.utils.ScienceUtils
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import CluProcessor._

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

  // the tokenizer
  lazy val tokenizer: Tokenizer = getArgString(s"$prefix.language", Some("EN")) match {
    case "PT" => new OpenDomainPortugueseTokenizer()
    case "ES" => new OpenDomainSpanishTokenizer()
    case _ => new OpenDomainEnglishTokenizer()
  }

  // the lemmatizer
  lazy val lemmatizer: Lemmatizer = getArgString(s"$prefix.language", Some("EN")) match {
    case "PT" => new PortugueseLemmatizer
    case "ES" => new SpanishLemmatizer
    case _ => new EnglishLemmatizer
  }

  override def annotate(doc:Document): Document = {
    // with this processor, we lemmatize first, because this POS tagger is part of the MTL framework
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

  /** Part of speech tagging */
  def tagPartsOfSpeech(doc:Document) {
    basicSanityCheck(doc)
    for(sent <- doc.sentences) {
    }
  }

  /** Lematization; modifies the document in place */
  def lemmatize(doc:Document) {
    basicSanityCheck(doc)
    for(sent <- doc.sentences) {
      //println(s"Lemmatize sentence: ${sent.words.mkString(", ")}")
      val lemmas = new Array[String](sent.size)
      for(i <- sent.words.indices) {
        lemmas(i) = lemmatizer.lemmatizeWord(sent.words(i))
        assert(lemmas(i).nonEmpty)
      }
      sent.lemmas = Some(lemmas)
    }
  }

  /** NER; modifies the document in place */
  def recognizeNamedEntities(doc:Document) {
  }

  /** Syntactic parsing; modifies the document in place */
  def parse(doc:Document) {
    basicSanityCheck(doc)
  }

  /** Shallow parsing; modifies the document in place */
  def chunking(doc:Document) {
  }

  /** Coreference resolution; modifies the document in place */
  def resolveCoreference(doc:Document) {
    // TODO
  }

  /** Discourse parsing; modifies the document in place */
  def discourse(doc:Document) {
    // TODO
  }

  /** Relation extraction; modifies the document in place. */
  override def relationExtraction(doc: Document): Unit = {
    // TODO
  }

  def basicSanityCheck(doc:Document): Unit = {
    if (doc.sentences == null)
      throw new RuntimeException("ERROR: Document.sentences == null!")
    if (doc.sentences.length != 0 && doc.sentences(0).words == null)
      throw new RuntimeException("ERROR: Sentence.words == null!")
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

  // overrided this because lemmatization depends on POS for portuguese
  override def annotate(doc:Document): Document = {
    cheapLemmatize(doc)
    tagPartsOfSpeech(doc)
    recognizeNamedEntities(doc)
    parse(doc)
    chunking(doc)
    lemmatize(doc)
    resolveCoreference(doc)
    discourse(doc)
    doc.clear()
    doc
  }

  // TODO:
  // make sure we are using the correct type of lemmas before running tagPartsOfSpeech(doc)
  // if we run tagPartsOfSpeech(doc) after lemmatize() and not after cheapLemmatize(doc)
  // we will get wrong results

  // generate cheap lemmas with the word in lower case for PartOfSpeech training/prediction only
  def cheapLemmatize(doc:Document) {
    basicSanityCheck(doc)
    for(sent <- doc.sentences) {
      // if not, generate cheap lemmas
      val lemmas = new Array[String](sent.size)
      for (i <- sent.words.indices) {
        lemmas(i) = sent.words(i).toLowerCase()
        assert(lemmas(i).nonEmpty)
      }
      sent.lemmas = Some(lemmas)
    }
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
          assert(lemmas(i).nonEmpty)
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


