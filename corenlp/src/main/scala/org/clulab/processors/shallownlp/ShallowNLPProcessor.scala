package org.clulab.processors.shallownlp

import java.util
import java.util.Properties
import java.util.zip.GZIPInputStream

import org.clulab.processors.corenlp.CoreNLPDocument
import org.clulab.processors.corenlp.chunker.CRFChunker
import org.clulab.processors._
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling.{CoreAnnotations, CoreLabel}
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.util.CoreMap

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import ShallowNLPProcessor._
import edu.stanford.nlp.naturalli.NaturalLogicAnnotations.RelationTriplesAnnotation
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.clu.tokenizer.{OpenDomainEnglishTokenizer, Tokenizer, TokenizerStep}
import org.clulab.struct.Interval

import scala.collection.JavaConverters._
import scala.util.matching.Regex

/**
  * A Processor using only shallow analysis: tokenization, lemmatization, POS tagging, and NER.
  * Tokenization is ours. All others implemented using Stanford's CoreNLP tools.
  * User: mihais
  * Date: 2/25/15
  * Last Modified: Update for Scala 2.12: java converters.
  * Last modified 06/11/2018: Switched to our internal tokenizer
  */
class ShallowNLPProcessor(val tokenizerPostProcessor:Option[TokenizerStep],
                          val internStrings:Boolean,
                          val withChunks:Boolean,
                          val withRelationExtraction:Boolean) extends Processor {

  def this(internStrings:Boolean = false,
           withChunks:Boolean = true, withRelationExtraction:Boolean = false) {
    this(None, internStrings, withChunks, withRelationExtraction)
  }

  // This strange construction is designed to allow subclasses access to the value of the tokenizer while
  // at the same time allowing them to override the value.
  // val tokenizer: Tokenizer = new ModifiedTokenizer(super.tokenizer)
  // does not work in a subclass because super.tokenizer is invalid.  Instead it needs to be something like
  // val tokenizer: Tokenizer = new ModifiedTokenizer(localTokenizer)
  protected lazy val localTokenizer: Tokenizer = new OpenDomainEnglishTokenizer(tokenizerPostProcessor)
  lazy val tokenizer: Tokenizer = localTokenizer
  lazy val posTagger: StanfordCoreNLP = mkPosTagger
  lazy val lemmatizer: StanfordCoreNLP = mkLemmatizer
  lazy val ner: StanfordCoreNLP = mkNer
  lazy val chunker: CRFChunker = mkChunker


  protected def newStanfordCoreNLP(props: Properties, enforceRequirements: Boolean = true): StanfordCoreNLP = {
    // Prevent knownLCWords from changing on us.  To be safe, this is added every time
    // because of potential caching of annotators.  Yes, the 0 must be a string.
    props.put("maxAdditionalKnownLCWords", "0")
    new StanfordCoreNLP(props, enforceRequirements)
  }

  def mkPosTagger: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "pos")
    newStanfordCoreNLP(props, enforceRequirements = false)
  }

  def mkLemmatizer: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "lemma")
    newStanfordCoreNLP(props, enforceRequirements = false)
  }

  def mkNer: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "ner")
    newStanfordCoreNLP(props, enforceRequirements = false)
  }

  def mkChunker: CRFChunker = {
    val path = "org/clulab/processors/corenlp/chunker/chunker.crf.gz"
    val is = getClass.getClassLoader.getResourceAsStream(path)
    val gzis = new GZIPInputStream(is)
    CRFChunker.load(gzis)
  }



  def in(s:String):String = {
    if (internStrings) Processor.internString(s)
    else s
  }

  def mkDocument(text:String, keepText:Boolean): Document = {
    // create the CLU document
    val doc = CluProcessor.mkDocument(tokenizer, text, keepText = true)

    // now create the CoreNLP document Annotation
    cluDocToCoreDoc(doc, keepText)
  }

  /* // TODO: remove me
  def arrayOrNone[T: ClassTag](b:ArrayBuffer[T]): Option[Array[T]] = {
    if (b.nonEmpty) new Some[Array[T]](b.toArray)
    else None
  }
  */

  def mkDocumentFromSentences(sentences:Iterable[String],
                              keepText:Boolean,
                              charactersBetweenSentences:Int = 1): Document = {
    // create the CLU document
    val doc = CluProcessor.mkDocumentFromSentences(tokenizer, sentences, keepText = true, charactersBetweenSentences)

    // now create the CoreNLP document Annotation
    cluDocToCoreDoc(doc, keepText)
  }

  def mkDocumentFromTokens(sentences:Iterable[Iterable[String]],
                           keepText:Boolean,
                           charactersBetweenSentences:Int = 1,
                           charactersBetweenTokens:Int = 1): Document = {
    // create the CLU document
    val doc = CluProcessor.mkDocumentFromTokens(sentences, keepText = true, charactersBetweenSentences, charactersBetweenTokens)

    // now create the CoreNLP document Annotation
    cluDocToCoreDoc(doc, keepText)
  }

  def basicSanityCheck(doc:Document, checkAnnotation:Boolean = true): Option[Annotation] = {
    if (doc.sentences == null)
      throw new RuntimeException("ERROR: Document.sentences == null!")
    if (doc.sentences.length == 0) return None
    if (doc.sentences(0).words == null)
      throw new RuntimeException("ERROR: Sentence.words == null!")

    if (checkAnnotation && doc.isInstanceOf[CoreNLPDocument]) {
      val annotation = doc.asInstanceOf[CoreNLPDocument].annotation.getOrElse(
        throw new RuntimeException("ERROR: annotator called after Document.clear()!"))
      Some(annotation)
    } else {
      None
    }
  }

  /**
   * Hook to allow postprocessing of CoreNLP POS tagging *in place*, overwriting original POS tags
   * This is useful for domain-specific corrections
   * @param annotation The CoreNLP annotation
   */
  def postprocessTags(annotation:Annotation): Unit = {
    val sas = annotation.get(classOf[SentencesAnnotation]).asScala

    // Make sure parens are tagged correctly
    sas.foreach{ sa =>
      val tas = sa.get(classOf[TokensAnnotation]).asScala.toList.toArray
      for(i <- tas.indices) {
        if(LEFT_PARENS.findFirstMatchIn(tas(i).word()).nonEmpty) {
          tas(i).setTag("-LRB-")
        } else if(RIGHT_PARENS.findFirstMatchIn(tas(i).word()).nonEmpty) {
          tas(i).setTag("-RRB-")
        }
      }
    }
  }

  val LEFT_PARENS: Regex = """^(\-LRB\-)|(\-LSB\-)|(-LCB-)|\(|\[|\{$""".r
  val RIGHT_PARENS: Regex = """^(\-RRB\-)|(\-RSB\-)|(-RCB-)|\)|\]|\}$""".r

  def tagPartsOfSpeech(doc:Document) {
    val annotation = basicSanityCheck(doc)
    if (annotation.isEmpty) return

    posTagger.annotate(annotation.get)

    postprocessTags(annotation.get)

    // convert CoreNLP Annotations to our data structures
    val sas = annotation.get.get(classOf[SentencesAnnotation]).asScala
    var offset = 0
    for (sa <- sas) {
      val tb = new ArrayBuffer[String]
      val tas = sa.get(classOf[TokensAnnotation]).asScala
      for (ta <- tas) {
        tb += in(ta.tag())
      }
      doc.sentences(offset).tags = Some(tb.toArray)
      offset += 1
    }
  }

  def lemmatize(doc:Document) {
    val annotation = basicSanityCheck(doc)
    if (annotation.isEmpty) return
    if (doc.sentences.head.tags.isEmpty)
      throw new RuntimeException("ERROR: you have to run the POS tagger before lemmatization!")

    lemmatizer.annotate(annotation.get)

    val sas = annotation.get.get(classOf[SentencesAnnotation]).asScala
    var offset = 0
    for (sa <- sas) {
      val tb = new ArrayBuffer[String]
      val tas = sa.get(classOf[TokensAnnotation]).asScala
      for (ta <- tas) {
        tb += in(ta.lemma())
      }
      doc.sentences(offset).lemmas = Some(tb.toArray)
      offset += 1
    }
  }

  def namedEntitySanityCheck(doc:Document):Option[Annotation] = {
    val annotation = basicSanityCheck(doc)
    if (annotation.isEmpty) return None
    if (doc.sentences.head.tags.isEmpty)
      throw new RuntimeException("ERROR: you have to run the POS tagger before NER!")
    if (doc.sentences.head.lemmas.isEmpty)
      throw new RuntimeException("ERROR: you have to run the lemmatizer before NER!")
    annotation
  }

  def recognizeNamedEntities(doc:Document) {
    val annotation = namedEntitySanityCheck(doc)
    if(annotation.isEmpty) return

    if(doc.getDCT.isDefined)
      annotation.get.set(classOf[DocDateAnnotation], doc.getDCT.get)

    try {
      ner.annotate(annotation.get)
    } catch {
      case e:Exception =>
        println("Caught NER exception!")
        println("Document:\n" + doc)
        throw e
    }

    // convert CoreNLP Annotations to our data structures
    val sas = annotation.get.get(classOf[SentencesAnnotation]).asScala
    var offset = 0
    for (sa <- sas) {
      val tb = new ArrayBuffer[String]
      val nb = new ArrayBuffer[String]
      val tas = sa.get(classOf[TokensAnnotation]).asScala
      for (ta <- tas) {
        tb += in(ta.ner())
        val n = ta.get(classOf[NormalizedNamedEntityTagAnnotation])
        //println(s"NORM: $n")
        if (n != null) nb += in(n)
        else nb += in("O")
      }

      //println("NORMS: " + nb.mkString(", "))

      doc.sentences(offset).entities = Some(tb.toArray)
      doc.sentences(offset).norms = Some(nb.toArray)
      offset += 1
    }
  }

  def parse(doc:Document): Unit = {
    // nothing here; see classes that extend this
  }

  override def relationExtraction(doc: Document): Unit  = {
    // nothing here; see classes that extend this
  }

  def chunking(doc:Document) {
    if (withChunks) {
      for (s <- doc.sentences) {
        val words = s.words
        val tags = s.tags.get
        val chunks = chunker.classify(words, tags)
        s.chunks = Some(chunks)
      }
    }
  }

  def resolveCoreference(doc:Document) {
    // nothing here; see classes that extend this
  }

  def discourse(doc:Document) {
    // nothing here; see classes that extend this
  }

  /** Semantic role labeling */
  override def srl(doc: Document): Unit = {
    // not implemented yet
  }
}

object ShallowNLPProcessor {
  val NO_DISCOURSE = 0 // discourse parsing is disabled
  val WITH_DISCOURSE = 1 // discourse parsing is enabled
  val JUST_EDUS = 2 // only the generation of elementary discourse units is enabled

  /**
    * Creates the CoreNLP Annotation corresponding to a CLU document containing only tokens
    */
  def docToAnnotation(doc:Document): Annotation = {
    assert(doc.text.nonEmpty)
    val docAnnotation = new Annotation(doc.text.get)
    val sentencesAnnotation = new util.ArrayList[CoreMap]()
    docAnnotation.set(classOf[SentencesAnnotation], sentencesAnnotation.asInstanceOf[java.util.List[CoreMap]])
    val docTokens = new util.ArrayList[CoreLabel]

    var sentOffset = 0
    var tokenOffset = 0
    for(sentence <- doc.sentences) {
      // construct the original CoreNLP tokens
      val crtTokens:util.List[CoreLabel] = new util.ArrayList[CoreLabel]()
      var tokOffset = 0
      for (i <- sentence.indices) {
        val crtTok = new CoreLabel()
        // Note: the CoreNLP token stores the .word not .raw strings! This is needed for its downstream components.
        crtTok.setWord(sentence.words(i))
        crtTok.setValue(sentence.words(i))
        crtTok.setBeginPosition(sentence.startOffsets(i))
        crtTok.setEndPosition(sentence.endOffsets(i))
        crtTok.setIndex(tokOffset + 1) // Stanford counts tokens starting from 1
        crtTok.setSentIndex(sentOffset) // Stanford counts sentences starting from 0...
        crtTokens.add(crtTok)
        tokOffset += 1
      }

      //
      // construct a CoreNLP sentence
      //

      // tokens
      val crtSent = new Annotation(sentence.getSentenceText)
      crtSent.set(classOf[TokensAnnotation], crtTokens)
      docTokens.addAll(crtTokens)

      // character offsets and actual text
      val sentStartOffset = sentence.startOffsets.head
      val sentEndOffset = sentence.endOffsets.last
      crtSent.set(classOf[CharacterOffsetBeginAnnotation], new Integer(sentStartOffset))
      crtSent.set(classOf[CharacterOffsetEndAnnotation], new Integer(sentEndOffset))
      crtSent.set(classOf[TextAnnotation], doc.text.get.substring(sentStartOffset, sentEndOffset))

      // token and sentence offsets
      crtSent.set(classOf[TokenBeginAnnotation], new Integer(tokenOffset))
      tokenOffset += crtTokens.size()
      crtSent.set(classOf[TokenEndAnnotation], new Integer(tokenOffset))
      crtSent.set(classOf[SentenceIndexAnnotation], new Integer(sentOffset)) // Stanford counts sentences starting from 0

      sentencesAnnotation.add(crtSent)
      sentOffset += 1
    }

    // document-wide annotations: all tokens in the doc, and the full text
    docAnnotation.set(classOf[CoreAnnotations.TokensAnnotation], docTokens)
    docAnnotation.set(classOf[CoreAnnotations.TextAnnotation], doc.text.get)

    docAnnotation
  }

  def cluDocToCoreDoc(doc:Document, keepText:Boolean): CoreNLPDocument = {
    val annotation = docToAnnotation(doc)
    val coreDoc = CoreNLPDocument(doc.sentences, annotation)
    if(keepText) coreDoc.text = doc.text
    coreDoc
  }

}
