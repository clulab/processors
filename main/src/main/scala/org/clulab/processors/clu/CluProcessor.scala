package org.clulab.processors.clu

import edu.knowitall.tool.stem.MorphaStemmer
import org.clulab.processors.clu.sequences.PartOfSpeechTagger
import org.clulab.processors.clu.syntax._
import org.clulab.processors.clu.tokenizer.{OpenDomainEnglishTokenizer, Tokenizer}
import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.struct.GraphMap
import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.processors.clu.bio._
import org.clulab.utils.Configured
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import CluProcessor._
import org.clulab.sequences.{LexiconNER, Tagger}

/**
  * Processor that uses only tools that are under Apache License
  * Currently supports:
  *   tokenization (in-house),
  *   lemmatization (Morpha),
  *   POS tagging (in-house BiMEMM),
  *   dependency parsing (ensemble of Malt models)
  */
class CluProcessor (val config: Config = ConfigFactory.load("cluprocessoropen")) extends Processor with Configured {

  override def getConf: Config = config

  // should we intern strings or not?
  val internStrings:Boolean = getArgBoolean(s"$prefix.internStrings", Some(false))

  // this class pre-processes the text before any computation happens
  lazy val tokenizerPreProcessor:Option[TokenizerPreProcessor] =
    getArgString(s"$prefix.tokenizer.pre.type", Some("none")) match {
      case "bio" => Some(new BioTokenizerPreProcessor(
        removeFigTabReferences = getArgBoolean(s"$prefix.tokenizer.pre.removeFigTabReferences", Some(true)),
        removeBibReferences = getArgBoolean(s"$prefix.tokenizer.pre.removeBibReferences", Some(true))))
      case "none" => None
      case _ => throw new RuntimeException(s"ERROR: Unknown argument value for $prefix.tokenizer.pre.type!")
    }

  // the actual tokenizer
  lazy val tokenizer: Tokenizer =
    new OpenDomainEnglishTokenizer

  // this class post-processes the tokens produced by the tokenizer
  lazy val tokenizerPostProcessor:Option[TokenizerPostProcessor] =
    getArgString(s"$prefix.tokenizer.post.type", Some("none")) match {
      case "bio" => Some(new BioTokenizerPostProcessor(
        getArgStrings(s"$prefix.tokenizer.post.tokensWithValidSlashes", None)
      ))
      case "none" => None
      case _ => throw new RuntimeException(s"ERROR: Unknown argument value for $prefix.tokenizer.post.type!")
    }

  // the POS tagger
  lazy val posTagger: PartOfSpeechTagger =
    PartOfSpeechTagger.loadFromResource(getArgString(s"$prefix.pos.model", None))

  // this class post-processes the POS tagger to avoid some common tagging mistakes for bio
  lazy val posPostProcessor: Option[SentencePostProcessor] =
    getArgString(s"$prefix.pos.post.type", Some("none")) match {
      case "bio" => Some(new BioPOSPostProcessor())
      case "none" => None
      case _ => throw new RuntimeException(s"ERROR: Unknown argument value for $prefix.pos.post.type!")
    }

  // the NER tagger
  lazy val ner: Option[Tagger[String]] =
    getArgString(s"$prefix.ner.type", Some("none")) match {
      case "bio" => Some(LexiconNER(
        getArgStrings(s"$prefix.ner.kbs", None),
        Some(getArgStrings(s"$prefix.ner.overrides", None)),
        new BioLexiconEntityValidator,
        new BioLexicalVariations,
        useLemmasForMatching = false,
        caseInsensitiveMatching = true
      ))
      case "none" => None
      case _ => throw new RuntimeException(s"ERROR: Unknown argument value for $prefix.ner.type!")
    }

  // this class post-processes the NER labels to avoid some common tagging mistakes (used in bio)
  lazy val nerPostProcessor: Option[SentencePostProcessor] =
    getArgString(s"$prefix.ner.post.type", Some("none")) match {
      case "bio" => Some(new BioNERPostProcessor(getArgString(s"$prefix.ner.post.stopListFile", None)))
      case "none" => None
      case _ => throw new RuntimeException(s"ERROR: Unknown argument value for $prefix.ner.post.stopListFile!")
    }

  // the dependency parser
  lazy val depParser: Parser =
    //new MaltWrapper(getArgString(s"$prefix.parser.model", None), internStrings)
    new EnsembleMaltParser(getArgStrings(s"$prefix.parser.models", None))


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
    if(tokenizerPreProcessor.nonEmpty) tokenizerPreProcessor.get.process(origText)
    else origText
  }

  private def postProcess(document: Document): Document = {
    if(tokenizerPostProcessor.isEmpty) return document
    val sentences = new ArrayBuffer[Sentence]()
    for(s <- document.sentences) {
      // this method is called before any real NLP happens, so POS tags, NER, etc. should be empty
      assert(s.tags.isEmpty)
      assert(s.lemmas.isEmpty)
      assert(s.entities.isEmpty)

      val tokens = mkPostProcessorTokens(s)
      val newTokens = tokenizerPostProcessor.get.process(tokens)
      sentences += mkSentence(newTokens)
    }

    // this method is called before any real NLP happens, so coref and discourse should be empty
    assert(document.coreferenceChains.isEmpty)
    assert(document.discourseTree.isEmpty)
    val d = new Document(sentences.toArray)
    d.text = document.text
    d
  }

  private def mkPostProcessorTokens(sentence: Sentence):Array[PostProcessorToken] = {
    val tokens = new Array[PostProcessorToken](sentence.size)
    for(i <- sentence.indices) {
      tokens(i) = PostProcessorToken(sentence.words(i), sentence.startOffsets(i), sentence.endOffsets(i))
    }
    tokens
  }

  private def mkSentence(tokens:Array[PostProcessorToken]): Sentence = {
    val words = new Array[String](tokens.length)
    val startOffsets = new Array[Int](tokens.length)
    val endOffsets = new Array[Int](tokens.length)
    for(i <- tokens.indices) {
      words(i) = tokens(i).word
      startOffsets(i) = tokens(i).beginPosition
      endOffsets(i) = tokens(i).endPosition
    }
    new Sentence(words, startOffsets, endOffsets)
  }

  /** Constructs a document of tokens from free text; includes sentence splitting and tokenization */
  def mkDocument(text:String, keepText:Boolean = false): Document = {
    val sents = tokenizer.tokenize(text)
    val doc = new Document(sents)
    if(keepText) doc.text = Some(text)
    postProcess(doc)
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
    postProcess(doc)
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
    postProcess(doc)
  }

  private def mkSep(size:Int):String = {
    val os = new mutable.StringBuilder
    for (_ <- 0 until size) os.append(" ")
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

      if(posPostProcessor.nonEmpty) {
        posPostProcessor.get.process(sent)
      }
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
    if(ner.nonEmpty) {
      basicSanityCheck(doc)
      for (sentence <- doc.sentences) {
        val labels = ner.get.find(sentence)
        sentence.entities = Some(labels)

        if(nerPostProcessor.nonEmpty) {
          nerPostProcessor.get.process(sentence)
        }
      }
    }
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
  def resolveCoreference(doc:Document) {
    // TODO
  }

  /** Discourse parsing; modifies the document in place */
  def discourse(doc:Document) {
    // TODO
  }

  private def basicSanityCheck(doc:Document): Unit = {
    if (doc.sentences == null)
      throw new RuntimeException("ERROR: Document.sentences == null!")
    if (doc.sentences.length != 0 && doc.sentences(0).words == null)
      throw new RuntimeException("ERROR: Sentence.words == null!")
  }

}

trait TokenizerPreProcessor {
  def process(text:String):String
}

trait TokenizerPostProcessor {
  def process(tokens:Array[PostProcessorToken]):Array[PostProcessorToken]
}

trait SentencePostProcessor {
  def process(sentence: Sentence)
}

class BioCluProcessor extends CluProcessor(config = ConfigFactory.load("cluprocessorbio"))

object CluProcessor {
  val logger:Logger = LoggerFactory.getLogger(classOf[CluProcessor])
  val prefix:String = "CluProcessor"
}