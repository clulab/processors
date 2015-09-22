package edu.arizona.sista.processors.shallownlp

import java.util
import java.util.Properties
import java.nio.file.Paths

import edu.arizona.sista.processors.corenlp.CoreNLPDocument
import edu.arizona.sista.processors.corenlp.chunker.CRFChunker
import edu.arizona.sista.processors._
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling.{CoreAnnotations, CoreLabel}
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.util.CoreMap
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag

/**
 * A Processor using only shallow analysis: tokenization, lemmatization, POS tagging, and NER. All implemented using Stanford's CoreNLP tools.
 * User: mihais
 * Date: 2/25/15
 */
class ShallowNLPProcessor(val internStrings:Boolean = true) extends Processor {
  lazy val tokenizerWithoutSentenceSplitting = mkTokenizerWithoutSentenceSplitting
  lazy val tokenizerWithSentenceSplitting = mkTokenizerWithSentenceSplitting
  lazy val posTagger = mkPosTagger
  lazy val lemmatizer = mkLemmatizer
  lazy val ner = mkNer
  // lazy val chunker = mkChunker

  def mkTokenizerWithoutSentenceSplitting: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "tokenize")
    props.put("tokenize.language", "English")
    new StanfordCoreNLP(props)
  }

  def mkTokenizerWithSentenceSplitting: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "tokenize, ssplit")
    props.put("tokenize.language", "English")
    new StanfordCoreNLP(props)
  }

  def mkPosTagger: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "pos")
    new StanfordCoreNLP(props, false)
  }

  def mkLemmatizer: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "lemma")
    new StanfordCoreNLP(props, false)
  }

  def mkNer: StanfordCoreNLP = {
    val props = new Properties()
    props.put("annotators", "ner")
    new StanfordCoreNLP(props, false)
  }

//   def mkChunker: CRFChunker = {
//     val url = getClass.getClassLoader.getResource("edu/arizona/sista/processors/corenlp/chunker/chunker.crf.gz")
//     val file = Paths.get(url.toURI).toFile
//     CRFChunker.load(file)
//   }

  /**
   * Hook to allow postprocessing of CoreNLP tokenization
   * This is useful for domain-specific corrections, such as the ones in BioNLPProcessor
   * If you change the tokens, make sure to store them back in the sentence!
   * @param originalTokens Input CoreNLP sentence
   * @return The modified tokens
   */
  def postprocessTokens(originalTokens:Array[CoreLabel]):Array[CoreLabel] = originalTokens

  private def postprocessTokens(sentence:CoreMap): java.util.List[CoreLabel] = {
    val origTokens = sentence.get(classOf[TokensAnnotation]).asScala.toArray

    val modifiedTokens = postprocessTokens(origTokens)

    // readjust CoreNLP indices to reflect the new token count; these are crucial for correct dependencies!
    var offset = 1 // Stanford counts tokens starting at 1
    for(token <- modifiedTokens) {
      token.setIndex(offset)
      offset += 1
    }

    val tokensAsJava = modifiedTokens.toList.asJava
    sentence.set(classOf[TokensAnnotation], tokensAsJava)

    tokensAsJava
  }


  def mkDocument(text:String, keepText:Boolean): Document = {
    val annotation = new Annotation(text)
    tokenizerWithSentenceSplitting.annotate(annotation)
    val sas = annotation.get(classOf[SentencesAnnotation])
    val sentences = new Array[Sentence](sas.size())
    var offset = 0
    for (sa <- sas) {
      sentences(offset) = mkSentence(sa)
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

    val doc = new CoreNLPDocument(sentences, Some(annotation))
    if(keepText) doc.text = Some(text)

    doc
  }

  def mkSentence(annotation:CoreMap): Sentence = {
    val tas = postprocessTokens(annotation)

    val wordBuffer = new ArrayBuffer[String]
    val startOffsetBuffer = new ArrayBuffer[Int]
    val endOffsetBuffer = new ArrayBuffer[Int]

    for (ta <- tas) {
      wordBuffer.add(in(ta.word))
      startOffsetBuffer += ta.beginPosition()
      endOffsetBuffer += ta.endPosition()
    }

    new Sentence(
      wordBuffer.toArray,
      startOffsetBuffer.toArray,
      endOffsetBuffer.toArray)
  }

  def in(s:String):String = {
    if (internStrings) Processor.internString(s)
    else s
  }

  def arrayOrNone[T: ClassTag](b:ArrayBuffer[T]): Option[Array[T]] = {
    if (b.size > 0) new Some[Array[T]](b.toArray)
    else None
  }

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
      val tmpAnnotation = new Annotation(sentence)
      tokenizerWithoutSentenceSplitting.annotate(tmpAnnotation)
      val crtTokens:java.util.List[CoreLabel] = postprocessTokens(tmpAnnotation)

      // construct a proper sentence, with token and character offsets adjusted to make the entire document consistent
      val crtSent = new Annotation(sentence)
      crtSent.set(classOf[TokensAnnotation], crtTokens)
      crtSent.set(classOf[TokenBeginAnnotation], new Integer(tokenOffset))
      tokenOffset += crtTokens.size()
      crtSent.set(classOf[TokenEndAnnotation], new Integer(tokenOffset))
      crtSent.set(classOf[CoreAnnotations.SentenceIndexAnnotation], new Integer(sentOffset)) // Stanford counts sentences starting from 0
      var i = 0
      while(i < crtTokens.size()) {
        val crtTok = crtTokens.get(i)
        crtTok.setBeginPosition(crtTok.beginPosition() + charOffset)
        crtTok.setEndPosition(crtTok.endPosition() + charOffset)
        crtTok.setIndex(i + 1) // Stanford counts tokens starting from 1
        crtTok.setSentIndex(sentOffset) // Stanford counts sentences starting from 0...
        i += 1
      }

      sentencesAnnotation.add(crtSent)
      docSents(sentOffset) = mkSentence(crtSent)

      charOffset += sentence.length + charactersBetweenSentences
      sentOffset += 1
    }

    val doc = new CoreNLPDocument(docSents, Some(docAnnotation))
    if(keepText) doc.text = Some(origText)

    doc
  }

  private def mkSep(size:Int):String = {
    val os = new mutable.StringBuilder
    for (i <- 0 until size) os.append(" ")
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

      val crtSent = new Annotation(sentenceTexts(sentOffset))
      crtSent.set(classOf[TokensAnnotation], crtTokens)
      crtSent.set(classOf[TokenBeginAnnotation], new Integer(tokenOffset))
      tokenOffset += crtTokens.size()
      crtSent.set(classOf[TokenEndAnnotation], new Integer(tokenOffset))

      sentencesAnnotation.add(crtSent)
      docSents(sentOffset) = mkSentence(crtSent)
      sentOffset += 1
    }

    val doc = new CoreNLPDocument(docSents, Some(docAnnotation))
    if(keepText) doc.text = Some(origText)

    doc
  }

  def basicSanityCheck(doc:Document, checkAnnotation:Boolean = true): Option[Annotation] = {
    if (doc.sentences == null)
      throw new RuntimeException("ERROR: Document.sentences == null!")
    if (doc.sentences.length == 0) return None
    if (doc.sentences(0).words == null)
      throw new RuntimeException("ERROR: Sentence.words == null!")

    if(checkAnnotation) {
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
    val sas = annotation.get.get(classOf[SentencesAnnotation])
    var offset = 0
    for (sa <- sas) {
      val tb = new ArrayBuffer[String]
      val tas = sa.get(classOf[TokensAnnotation])
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
    if (doc.sentences.head.tags == None)
      throw new RuntimeException("ERROR: you have to run the POS tagger before lemmatization!")

    lemmatizer.annotate(annotation.get)

    val sas = annotation.get.get(classOf[SentencesAnnotation])
    var offset = 0
    for (sa <- sas) {
      val tb = new ArrayBuffer[String]
      val tas = sa.get(classOf[TokensAnnotation])
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
    if (doc.sentences.head.tags == None)
      throw new RuntimeException("ERROR: you have to run the POS tagger before NER!")
    if (doc.sentences.head.lemmas == None)
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
    val sas = annotation.get.get(classOf[SentencesAnnotation])
    var offset = 0
    for (sa <- sas) {
      val tb = new ArrayBuffer[String]
      val nb = new ArrayBuffer[String]
      val tas = sa.get(classOf[TokensAnnotation])
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
    // nothing here
  }

  def chunking(doc:Document) {
    // for (s <- doc.sentences) {
    //   val words = s.words
    //   val tags = s.tags.get
    //   val chunks = chunker.classify(words, tags)
    //   s.chunks = Some(chunks)
    // }
  }

  def labelSemanticRoles(doc:Document) {
    // nothing here
  }

  def resolveCoreference(doc:Document) {
    // nothing here
  }

  def discourse(doc:Document) {
    // nothing here
  }
}
