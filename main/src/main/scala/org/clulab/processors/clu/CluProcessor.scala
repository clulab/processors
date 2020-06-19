package org.clulab.processors.clu

import org.clulab.processors.clu.tokenizer._
import org.clulab.processors.{Document, IntermediateDocumentAttachment, Processor, Sentence}
import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.utils.Configured
import org.clulab.utils.ScienceUtils
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import CluProcessor._
import org.clulab.dynet.{AnnotatedSentence, Metal}
import org.clulab.struct.{DirectedGraph, Edge, GraphMap}

/**
  * Processor that uses only tools that are under Apache License
  * Currently supports:
  *   tokenization (in-house),
  *   lemmatization (Morpha, copied in our repo to minimize dependencies),
  *   POS tagging, NER, chunking, dependency parsing - using our MTL architecture (dep parsing coming soon)
  */
class CluProcessor (threads: Int = 1, val config: Config = ConfigFactory.load("cluprocessor")) extends Processor with Configured {

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

  override def annotate(doc:Document): Document = {
    tagPartsOfSpeech(doc) // the call to the syntax MTL is in here
    recognizeNamedEntities(doc) // the call to the NER MTL is in here
    chunking(doc) // Nothing, kept for the record
    parse(doc) // Nothing, kept for the record

    lemmatize(doc) // lemmatization has access to POS tags, which are needed in some languages

    srl(doc) // SRL (arguments)

    // these are not implemented yet
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

  class PredicateAttachment(val predicates: IndexedSeq[IndexedSeq[Int]]) extends IntermediateDocumentAttachment

  import scala.collection.parallel.ParSeq
  def parallelize[T](seq: Seq[T], threads: Int = threads): ParSeq[T] = {
    import scala.collection.parallel.ForkJoinTaskSupport
    import scala.collection.parallel.ForkJoinTasks

    val forkJoinPoolConstructor = {
      // Get something of the right type.
      val defaultForkJoinPool = ForkJoinTasks.defaultForkJoinPool
      // Find the constructor.
      defaultForkJoinPool.getClass.getConstructor(classOf[Int])
    }

    def newForkJoinPool(threads: Int) = {
      // Invoke the constructor.
      forkJoinPoolConstructor.newInstance(threads.asInstanceOf[Integer])

      // For the record, this is the standard version
      //new ForkJoinPool(threads)
    }

    val forkJoinPool = newForkJoinPool(threads)
    val forkJoinTaskSupport = new ForkJoinTaskSupport(forkJoinPool)
    val parSeq = seq.par

    parSeq.tasksupport = forkJoinTaskSupport
    parSeq
  }

  /** Part of speech tagging + chunking + SRL (predicates), jointly */
  override def tagPartsOfSpeech(doc:Document) {
    basicSanityCheck(doc)

    // Test to see if it's working.
    parallelize(0.until(1000)).foreach { index =>
      println(index)
    }

    val sentences = parallelize(doc.sentences)

    val preds = sentences.map { sent =>
      val allLabels = mtlPosChunkSrlp.predictJointly(new AnnotatedSentence(sent.words))
      sent.tags = Some(allLabels(0).toArray)
      sent.chunks = Some(allLabels(1).toArray)

      // get the index of all predicates in this sentence
      val predsInSent = new ArrayBuffer[Int]()
      var done = false
      var offset = 0
      while(! done) {
        val idx = allLabels(2).indexOf("B-P", offset)

        if(idx >= 0) {
          predsInSent += idx
          offset = idx + 1
        } else {
          done = true
        }
      }
      predsInSent
    }.toIndexedSeq

    // store the index of all predicates as a doc attachment
    doc.addAttachment(PREDICATE_ATTACHMENT_NAME, new PredicateAttachment(preds))
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

    val sentences = parallelize(doc.sentences)

    sentences.foreach { sent =>
      val allLabels = mtlNer.predictJointly(new AnnotatedSentence(sent.words))
      sent.entities = Some(allLabels(0).toArray)
    }
  }

  override def srl(doc: Document): Unit = {
    val predicatesAttachment = doc.getAttachment(PREDICATE_ATTACHMENT_NAME)
    assert(predicatesAttachment.nonEmpty)

    if(doc.sentences.length > 0) {
      assert(doc.sentences(0).tags.nonEmpty)
      assert(doc.sentences(0).entities.nonEmpty)
    }

    val predicates = predicatesAttachment.get.asInstanceOf[PredicateAttachment].predicates
    assert(predicates.length == doc.sentences.length)

    val indices = parallelize(predicates.indices)

    // generate SRL frames for each predicate in each sentence
    indices.foreach { si =>
      val sentence = doc.sentences(si)

      val edges = new ListBuffer[Edge[String]]()
      val roots = new mutable.HashSet[Int]()

      // SRL needs POS tags and NEs!
      val annotatedSentence =
        new AnnotatedSentence(sentence.words,
          Some(sentence.tags.get), Some(sentence.entities.get))

      // all predicates become roots
      val preds = predicates(si)
      roots ++= preds

      for(pi <- preds.indices) {
        val pred = preds(pi)
        val argLabels = mtlSrla.predict(0, annotatedSentence, Some(pred))

        for(ai <- argLabels.indices) {
          if(argLabels(ai) != "O") {
            val edge = Edge[String](pred, ai, argLabels(ai))
            edges += edge
          }
        }
      }

      val sentGraph = new DirectedGraph[String](edges.toList, roots.toSet)
      sentence.graphs += GraphMap.SEMANTIC_ROLES -> sentGraph
    }

    doc.removeAttachment(PREDICATE_ATTACHMENT_NAME)
  }

  /** Syntactic parsing; modifies the document in place */
  def parse(doc:Document): Unit = {
    // Nop, covered by MTL
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


