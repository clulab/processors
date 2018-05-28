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
import edu.stanford.nlp.process.CoreLabelTokenFactory
import edu.stanford.nlp.util.CoreMap
import org.clulab.processors.clu.PostProcessorToken

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

import ShallowNLPProcessor._

//
// TODO: call token normalization from CluProcessor
//

/**
  * A Processor using only shallow analysis: tokenization, lemmatization, POS tagging, and NER.
  * All implemented using Stanford's CoreNLP tools.
  * User: mihais
  * Date: 2/25/15
  * Last Modified: Update for Scala 2.12: java converters.
  */
class ShallowNLPProcessor(val internStrings:Boolean = true, val withChunks:Boolean = true) extends Processor {
  lazy val tokenizerWithoutSentenceSplitting: StanfordCoreNLP = mkTokenizerWithoutSentenceSplitting
  lazy val tokenizerWithSentenceSplitting: StanfordCoreNLP = mkTokenizerWithSentenceSplitting
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

  def mkTokenizerWithoutSentenceSplitting: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "tokenize")
    props.put("tokenize.language", "English")
    newStanfordCoreNLP(props)
  }

  def mkTokenizerWithSentenceSplitting: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "tokenize, ssplit")
    props.put("tokenize.language", "English")
    newStanfordCoreNLP(props)
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

  /**
   * Hook to allow postprocessing of CoreNLP tokenization
   * This is useful for domain-specific corrections, such as the ones in BioNLPProcessor
   * If you change the tokens, make sure to store them back in the sentence!
   * @param originalTokens Input CoreNLP sentence
   * @return The modified tokens
   */
  def postprocessTokens(originalTokens:Array[CoreLabel]):Array[PostProcessorToken] =
    coreLabelsToPostProcessorTokens(originalTokens)

  def mkDocument(text:String, keepText:Boolean): Document = {
    // tokenization
    val annotation = new Annotation(text)
    tokenizerWithSentenceSplitting.annotate(annotation)
    val sas = annotation.get(classOf[SentencesAnnotation]).asScala

    // construct CLU sentences
    val sentences = new Array[Sentence](sas.size)
    var offset = 0
    for (sa <- sas) {
      // tokenization post-processing
      val ppts = postprocessTokens(getCoreMapTokens(sa))

      // construct the CLU sentence
      sentences(offset) = postProcessorTokensToSentence(ppts)

      // re-construct the tokens in the CoreNLP sentence
      val tokensAsJava = postProcessorTokensToCoreLabels(ppts).toList.asJava
      sa.set(classOf[TokensAnnotation], tokensAsJava)

      offset += 1
    }

    // just in case the postprocessing code changed token offsets, reset them
    var tokenOffset = 0
    for(sa <- sas) {
      val crtTokens = sa.get(classOf[TokensAnnotation])
      sa.set(classOf[TokenBeginAnnotation], new Integer(tokenOffset))
      tokenOffset += crtTokens.size()
      sa.set(classOf[TokenEndAnnotation], new Integer(tokenOffset))
    }

    val doc = CoreNLPDocument(sentences, annotation)
    if(keepText) doc.text = Some(text)

    doc
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
    val origText = sentences.mkString(mkSep(charactersBetweenSentences))
    val docAnnotation = new Annotation(origText)
    val sentencesAnnotation = new util.ArrayList[CoreMap]()
    docAnnotation.set(classOf[SentencesAnnotation], sentencesAnnotation.asInstanceOf[java.util.List[CoreMap]])
    val docSents = new Array[Sentence](sentences.size)

    var sentOffset = 0
    var charOffset = 0
    var tokenOffset = 0
    for(sentence <- sentences) {
      // basic tokenization
      val tmpAnnotation = new Annotation(sentence)
      tokenizerWithoutSentenceSplitting.annotate(tmpAnnotation)

      // tokenization post-processing
      val postProcessorTokens = postprocessTokens(getCoreMapTokens(tmpAnnotation))
      val coreNLPTokens:java.util.List[CoreLabel] = postProcessorTokensToCoreLabels(postProcessorTokens).toList.asJava

      // construct a proper CoreNLP sentence, with token and character offsets adjusted to make the entire document consistent
      val crtSent = new Annotation(sentence)
      crtSent.set(classOf[TokensAnnotation], coreNLPTokens)
      crtSent.set(classOf[TokenBeginAnnotation], new Integer(tokenOffset))
      tokenOffset += coreNLPTokens.size()
      crtSent.set(classOf[TokenEndAnnotation], new Integer(tokenOffset))
      crtSent.set(classOf[CoreAnnotations.SentenceIndexAnnotation], new Integer(sentOffset)) // Stanford counts sentences starting from 0
      var i = 0
      while(i < coreNLPTokens.size()) {
        val crtTok = coreNLPTokens.get(i)
        crtTok.setBeginPosition(crtTok.beginPosition() + charOffset)
        crtTok.setEndPosition(crtTok.endPosition() + charOffset)
        crtTok.setIndex(i + 1) // Stanford counts tokens starting from 1
        crtTok.setSentIndex(sentOffset) // Stanford counts sentences starting from 0...
        i += 1
      }
      sentencesAnnotation.add(crtSent)

      // construct a CLU sentence
      docSents(sentOffset) = postProcessorTokensToSentence(postProcessorTokens)

      charOffset += sentence.length + charactersBetweenSentences
      sentOffset += 1
    }

    val doc = CoreNLPDocument(docSents, docAnnotation)
    if(keepText) doc.text = Some(origText)

    doc
  }

  private def mkSep(size:Int):String = {
    val os = new mutable.StringBuilder
    for (_ <- 0 until size) os.append(" ")
    os.toString()
  }

  def mkDocumentFromTokens(sentences:Iterable[Iterable[String]],
                           keepText:Boolean,
                           charactersBetweenSentences:Int = 1,
                           charactersBetweenTokens:Int = 1): Document = {
    val sb = new ListBuffer[String]
    for (s <- sentences)
      sb += s.mkString(mkSep(charactersBetweenTokens))
    val sentenceTexts = sb.toArray
    val origText = sentenceTexts.mkString(mkSep(charactersBetweenSentences))
    val docAnnotation = new Annotation(origText)
    val sentencesAnnotation = new util.ArrayList[CoreMap]()
    docAnnotation.set(classOf[SentencesAnnotation], sentencesAnnotation.asInstanceOf[java.util.List[CoreMap]])
    val docSents = new Array[Sentence](sentences.size)

    var sentOffset = 0
    var charOffset = 0
    var tokenOffset = 0
    for(sentence <- sentences) {
      // construct the original CoreNLP tokens
      val crtTokens:util.List[CoreLabel] = new util.ArrayList[CoreLabel]()
      var tokOffset = 0
      for (w <- sentence) {
        val crtTok = new CoreLabel()
        crtTok.setWord(w)
        crtTok.setValue(w)
        crtTok.setBeginPosition(charOffset)
        charOffset += w.length
        crtTok.setEndPosition(charOffset)
        crtTok.setIndex(tokOffset + 1) // Stanford counts tokens starting from 1
        crtTok.setSentIndex(sentOffset) // Stanford counts sentences starting from 0...
        crtTokens.add(crtTok)
        tokOffset += 1
        charOffset += charactersBetweenTokens
      }

      // construct a CoreNLP sentence
      val crtSent = new Annotation(sentenceTexts(sentOffset))
      crtSent.set(classOf[TokensAnnotation], crtTokens)
      crtSent.set(classOf[TokenBeginAnnotation], new Integer(tokenOffset))
      tokenOffset += crtTokens.size()
      crtSent.set(classOf[TokenEndAnnotation], new Integer(tokenOffset))
      sentencesAnnotation.add(crtSent)

      // tokenization post-processing
      val ppts = postprocessTokens(getCoreMapTokens(crtSent))

      // construct the CLU sentence
      docSents(sentOffset) = postProcessorTokensToSentence(ppts)

      // re-construct the tokens in the CoreNLP sentence
      val tokensAsJava = postProcessorTokensToCoreLabels(ppts).toList.asJava
      crtSent.set(classOf[TokensAnnotation], tokensAsJava)

      sentOffset += 1
    }

    val doc = CoreNLPDocument(docSents, docAnnotation)
    if(keepText) doc.text = Some(origText)

    doc
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
  def postprocessTags(annotation:Annotation) { }

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
        if (n != null) nb += in(n)
        else nb += in("O")
      }
      doc.sentences(offset).entities = Some(tb.toArray)
      doc.sentences(offset).norms = Some(nb.toArray)
      offset += 1
    }
  }

  def parse(doc:Document): Unit = {
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
}

object ShallowNLPProcessor {
  val NO_DISCOURSE = 0 // discourse parsing is disabled
  val WITH_DISCOURSE = 1 // discourse parsing is enabled
  val JUST_EDUS = 2 // only the generation of elementary discourse units is enabled

  /**
    * Converts an array of CoreLabel to an array of PostProcessorToken
    * Note that this takes place *before* post-processing, so the .raw == .word at this stage
    * @param labels the Stanford CoreNLP tokenized words
    * @return Same data organized as PostProcessorToken
    */
  def coreLabelsToPostProcessorTokens(labels: Array[CoreLabel]): Array[PostProcessorToken] = {
    val tokens = new Array[PostProcessorToken](labels.length)
    for(i <- labels.indices) {
      tokens(i) = PostProcessorToken(labels(i).word(),
        labels(i).beginPosition(), labels(i).endPosition(), labels(i).word())
    }
    tokens
  }

  /**
    * Converts an array of PostProcessorToken to an array of CoreLabel
    * Note: this uses the .word fields (rather than .raw) to construct the CoreLabel
    * @param tokens the tokens produced by the post-processor
    * @return The same tokens as CoreLabels
    */
  def postProcessorTokensToCoreLabels(tokens: Array[PostProcessorToken]): Array[CoreLabel] = {
    val labels = new Array[CoreLabel](tokens.length)
    val f = new CoreLabelTokenFactory()

    // readjust CoreNLP indices to reflect the new token count; these are crucial for correct dependencies!
    var offset = 1 // Stanford counts tokens starting at 1

    for(i <- tokens.indices) {
      val l = f.makeToken(
        tokens(i).word, // note that we use word rather than raw here!
        tokens(i).beginPosition,
        tokens(i).endPosition - tokens(i).beginPosition)
      l.setIndex(offset)
      labels(i) = l
      offset += 1
    }

    labels
  }

  /** Fetches the tokens stored inside a CoreNLP sentence */
  def getCoreMapTokens(sentence:CoreMap): Array[CoreLabel] =
    sentence.get(classOf[TokensAnnotation]).asScala.toArray

  /** Converts a CoreNLP sentence to a CLU sentence */
  def coreMapToSentence(sentence:CoreMap): Sentence = {
    val tokens = getCoreMapTokens(sentence)

    val rawBuffer = new ArrayBuffer[String]()
    val wordBuffer = new ArrayBuffer[String]
    val startOffsetBuffer = new ArrayBuffer[Int]
    val endOffsetBuffer = new ArrayBuffer[Int]

    for (token <- tokens) {
      wordBuffer += token.word()
      rawBuffer += token.word()
      startOffsetBuffer += token.beginPosition()
      endOffsetBuffer += token.endPosition()
    }

    Sentence(
      rawBuffer.toArray,
      startOffsetBuffer.toArray,
      endOffsetBuffer.toArray,
      wordBuffer.toArray
    )
  }

  /** Converts the output of the tokenization post-processing to a CLU sentence */
  def postProcessorTokensToSentence(tokens:Array[PostProcessorToken]): Sentence = {
    val rawBuffer = new ArrayBuffer[String]()
    val wordBuffer = new ArrayBuffer[String]
    val startOffsetBuffer = new ArrayBuffer[Int]
    val endOffsetBuffer = new ArrayBuffer[Int]

    for (token <- tokens) {
      wordBuffer += token.word
      rawBuffer += token.raw
      startOffsetBuffer += token.beginPosition
      endOffsetBuffer += token.endPosition
    }

    Sentence(
      rawBuffer.toArray,
      startOffsetBuffer.toArray,
      endOffsetBuffer.toArray,
      wordBuffer.toArray
    )
  }
}
