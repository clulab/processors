package org.clulab.processors.clu

import java.io.StringReader

import org.clulab.processors.clu.sequences.{Chunker, NamedEntityRecognizer, PartOfSpeechTagger}
import org.clulab.processors.clu.syntax._
import org.clulab.processors.clu.tokenizer.{OpenDomainEnglishTokenizer, Tokenizer}
import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.struct.GraphMap
import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.processors.clu.bio._
import org.clulab.utils.{Configured, ScienceUtils}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import CluProcessor._
import org.clulab.sequences.{LexiconNER, Tagger}
import uk.ac.susx.informatics.Morpha

import scala.util.matching.Regex

/**
  * Processor that uses only tools that are under Apache License
  * Currently supports:
  *   tokenization (in-house),
  *   lemmatization (Morpha, copied in our repo to minimize dependencies),
  *   POS tagging (in-house BiMEMM),
  *   dependency parsing (ensemble of Malt models)
  */
class CluProcessor (val config: Config = ConfigFactory.load("cluprocessoropen")) extends Processor with Configured {

  override def getConf: Config = config

  // should we intern strings or not?
  val internStrings:Boolean = getArgBoolean(s"$prefix.internStrings", Some(false))

  // the actual tokenizer
  lazy val tokenizer: Tokenizer =
    new OpenDomainEnglishTokenizer

  // used for text normalization
  lazy val scienceUtils = new ScienceUtils

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
      case "conll" => Some(NamedEntityRecognizer.loadFromResource(getArgString(s"$prefix.ner.model", None)))
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

  // the syntactic chunker
  lazy val chunker:Chunker =
    Chunker.loadFromResource(getArgString(s"$prefix.chunker.model", None))

  // should we use universal dependencies or Stanford ones?
  val useUniversalDependencies:Boolean = getArgBoolean(s"$prefix.parser.universal", Some(true))

  // the dependency parser
  lazy val depParser: Parser =
    if(useUniversalDependencies) {
      //new MaltWrapper(getArgString(s"$prefix.parser.model", None), internStrings)
      new EnsembleMaltParser(getArgStrings(s"$prefix.parser.models-universal", None))
    } else {
      new EnsembleMaltParser(getArgStrings(s"$prefix.parser.models-stanford", None))
    }


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

  private def postProcess(document: Document): Document = {
    //
    // apply domain specific tokenization rules (see BioTokenizerPostProcessor for such an example)
    //
    val postProcessedDoc =
      if(tokenizerPostProcessor.isEmpty) {
        return document
      } else {
        val sentences = new ArrayBuffer[Sentence]()
        for (s <- document.sentences) {
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

    //
    // normalize raw tokens into words
    // this includes:
    // - replacing common Unicode characters with the corresponding ASCII string, e.g., \u0277 is replaced with "omega"
    // - replacing parens with -LRB-, -RRB-, etc.
    //
    for(sentence <- postProcessedDoc.sentences) {
      // this assumes that sentence.words has been populated already!
      assert(sentence.words != null)
      assert(sentence.words.length == sentence.size)

      for(i <- 0 until sentence.size) {
        val w = sentence.words(i)
        assert(w != null)

        // replace unicode
        sentence.words(i) = scienceUtils.replaceUnicodeWithAscii(w)

        // replace parens
        sentence.words(i) =
          sentence.words(i) match {
            case "(" => "-LRB-"
            case ")" => "-RRB-"
            case "[" => "-LSB-"
            case "]" => "-RSB-"
            case w:String => w
          }
      }
    }

    postProcessedDoc
  }

  private def mkPostProcessorTokens(sentence: Sentence):Array[PostProcessorToken] = {
    val tokens = new Array[PostProcessorToken](sentence.size)
    for(i <- sentence.indices) {
      // at this stage, .raw == .word in the PostProcessorToken
      tokens(i) = PostProcessorToken(
        sentence.raw(i),
        sentence.startOffsets(i), sentence.endOffsets(i),
        sentence.raw(i))
    }
    tokens
  }

  private def mkSentence(tokens:Array[PostProcessorToken]): Sentence = {
    val raw = new Array[String](tokens.length)
    val words = new Array[String](tokens.length)
    val startOffsets = new Array[Int](tokens.length)
    val endOffsets = new Array[Int](tokens.length)
    for(i <- tokens.indices) {
      raw(i) = tokens(i).raw
      words(i) = tokens(i).word
      startOffsets(i) = tokens(i).beginPosition
      endOffsets(i) = tokens(i).endPosition
    }
    new Sentence(raw, startOffsets, endOffsets, words)
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
      sents += new Sentence(sentence.toArray, startOffsets.toArray, endOffsets.toArray, sentence.toArray)
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

  def lemmatizeWord(word:String):String = {
    if(parens.findFirstMatchIn(word).nonEmpty)
      return word

    val norm = CluProcessor.normalizeForLemmatization(word).trim
    if(norm.isEmpty) return ""
    
    val parts = norm.split(whitespaces)

    val result = new mutable.StringBuilder()
    for(part <- parts) {
      val morpha = new Morpha(new StringReader(part), false)

      var lemma = part
      try {
        lemma = morpha.next()
      } catch {
        case _:Throwable =>
      }

      if(result.length > 0) result.append(" ")
      result.append(lemma)
    }
    result.toString()
  }

  /** Lematization; modifies the document in place */
  def lemmatize(doc:Document) {
    basicSanityCheck(doc)
    for(sent <- doc.sentences) {
      //println(s"Lemmatize sentence: ${sent.words.mkString(", ")}")
      val lemmas = new Array[String](sent.size)
      for(i <- sent.words.indices) {
        var lemma = lemmatizeWord(sent.words(i))
        // in some cases, Morpha returns empty strings
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
      //println(s"PARSING SENTENCE: ${sentence.words.mkString(", ")}")
      val dg = depParser.parseSentence(sentence)

      if(useUniversalDependencies) {
        sentence.setDependencies(GraphMap.UNIVERSAL_BASIC, dg)
        sentence.setDependencies(GraphMap.UNIVERSAL_ENHANCED,
          EnhancedDependencies.generateUniversalEnhancedDependencies(sentence, dg))
      } else {
        sentence.setDependencies(GraphMap.STANFORD_BASIC, dg)
        sentence.setDependencies(GraphMap.STANFORD_COLLAPSED,
          EnhancedDependencies.generateStanfordEnhancedDependencies(sentence, dg))
      }
    }
  }

  /** Shallow parsing; modifies the document in place */
  def chunking(doc:Document) {
    basicSanityCheck(doc)
    for(sent <- doc.sentences) {
      val chunks = chunker.classesOf(sent)
      sent.chunks = Some(chunks)
    }
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

trait TokenizerPostProcessor {
  def process(tokens:Array[PostProcessorToken]):Array[PostProcessorToken]
}

case class PostProcessorToken(raw:String, beginPosition:Int, endPosition:Int, word:String)

object PostProcessorToken {
  def mkWithLength(raw:String, beginPosition:Int, word:String): PostProcessorToken = {
    PostProcessorToken(raw, beginPosition, beginPosition + raw.length, word)
  }
}

trait SentencePostProcessor {
  def process(sentence: Sentence)
}

class BioCluProcessor extends CluProcessor(config = ConfigFactory.load("cluprocessorbio"))

class CluProcessorWithStanford extends CluProcessor(config = ConfigFactory.load("cluprocessoropenwithstanford"))

object CluProcessor {
  val logger:Logger = LoggerFactory.getLogger(classOf[CluProcessor])
  val prefix:String = "CluProcessor"

  /** Special characters to remove. */
  val remove: Regex = """[()\[\].,;:"']""".r

  val parens: Regex = """^(\-LRB\-)|(\-RRB\-)|(\-LSB\-)|(-RSB-)$""".r

  /** White spaces */
  val whitespaces: String = "\\s+"

  /** Remove special characters and lowercase the string. */
  def normalizeForLemmatization(word: String):String = CluProcessor.remove.replaceAllIn(word.trim.toLowerCase, "")
}


