package org.clulab.processors.coserver

import scala.concurrent.duration._

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.actor.{ ActorSystem, Props, Actor }
import akka.testkit.{ TestKit, TestActorRef, TestProbe, ImplicitSender }
import org.scalatest.{ BeforeAndAfterAll, FlatSpecLike, MustMatchers }

import org.clulab.processors._
import org.clulab.processors.bionlp._
import org.clulab.processors.corenlp._
import org.clulab.processors.fastnlp._
import org.clulab.processors.shallownlp._
import org.clulab.processors.coshare.ProcessorCoreMessages._

/**
  * Unit tests of the ProcessorActor class.
  *   Written by: Tom Hicks. 6/6/2017.
  *   Last Modified: Refactor for sharing.
  */
class TestProcessorActor extends TestKit(ActorSystem("test-proc-actor"))
    with FlatSpecLike
    with ImplicitSender
    with BeforeAndAfterAll
    with MustMatchers
    with LazyLogging
{
  val config = ConfigFactory.load().getConfig("ProcessorCoreServer")

  // read which processor type is specified by the configuration
  val procType = config.getString("server.processor.type")
  logger.debug(s"processor type=${procType}")

  // set default timeout: raise it for some processors (below)
  var timeout: FiniteDuration = 30.seconds

  // set default use of co-reference to false: only CoreNLP seems to use it
  var usesCoreference = false

  // create the Processor engine used by this server. increase timeout for slow processors.
  val processor: Processor = {
    procType.toLowerCase match {
      case "bio" =>
        timeout = 3.minutes
        new BioNLPProcessor(removeFigTabReferences = true)

      case "core" =>
        usesCoreference = true
        new CoreNLPProcessor()

      case "fast" =>
        new FastNLPProcessor()

      case "fastbio" =>
        timeout = 3.minutes
        new FastBioNLPProcessor(removeFigTabReferences = true)

      case _ => new ShallowNLPProcessor()
    }
  }

  val procActor = system.actorOf(ProcessorActor.props(processor))
  logger.debug(s"actor=${procActor}")


  override def afterAll = {
    TestKit.shutdownActorSystem(system)
  }

  // ErrorTest
  "ProcessorActor" should "return error upon error test" in {
    val probe = TestProbe()
    probe.send(procActor, ErrorTestCmd())
    val reply = probe.expectMsgClass(timeout, classOf[ServerExceptionMsg])
    (reply) must not be (null)
    (reply.exception) must not be (null)
    (reply.exception.isInstanceOf[RuntimeException]) must be (true)
    (reply.exception.getMessage()) must be ("This is a fake error from the ErrorTest command.")
  }

  // error on unknown message
  it should "return error upon unknown message" in {
    val probe = TestProbe()
    probe.send(procActor, "UNKNOWN")
    val reply = probe.expectMsgClass(timeout, classOf[ServerExceptionMsg])
    (reply) must not be (null)
    (reply.exception) must not be (null)
    (reply.exception.isInstanceOf[RuntimeException]) must be (true)
    (reply.exception.getMessage()) must be ("ProcessorActor: received unrecognized message: UNKNOWN")
  }


  // mkDocument
  "ProcessorActor" should "make document from zero-length text, keep text" in {
    val probe = TestProbe()
    val text = ""
    probe.send(procActor, MkDocumentCmd(text, true)) // keep text
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(0)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(text))
  }

  it should "make document from simple text, default keep" in {
    val probe = TestProbe()
    val text = "This is a test."
    probe.send(procActor, MkDocumentCmd(text)) // default keep
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(1)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

  it should "make document from simple text, keep text" in {
    val probe = TestProbe()
    val text = "This is a test."
    probe.send(procActor, MkDocumentCmd(text, true)) // keep text
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(1)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(text))
  }

  it should "make document from simple text, discard text" in {
    val probe = TestProbe()
    val text = "This is a test."
    probe.send(procActor, MkDocumentCmd(text, false)) // discard text
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(1)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

  // mkDocumentFromSentences
  it should "make document from sentences, keep text" in {
    val probe = TestProbe()
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    probe.send(procActor, MkDocumentFromSentencesCmd(sents, true)) // keep text
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(3)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(sents.mkString(" "))) // spacing: sent=1
  }

  it should "make document from sentences, discard text" in {
    val probe = TestProbe()
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    probe.send(procActor, MkDocumentFromSentencesCmd(sents, false)) // discard text
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(3)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

  it should "make document from sentences, keep text, add extra spacing" in {
    val probe = TestProbe()
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    probe.send(procActor, MkDocumentFromSentencesCmd(sents, true, 3))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(3)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(sents.mkString("   "))) // spacing: sent=3
  }

  // mkDocumentFromTokens
  it should "make document from tokens, keep text" in {
    val probe = TestProbe()
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val text = toks.map(t => t.mkString(" ")).mkString(" ") // spacing: tok=1, sent=1
    probe.send(procActor, MkDocumentFromTokensCmd(toks, true))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(2)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(text))
  }

  it should "make document from tokens, discard text" in {
    val probe = TestProbe()
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    probe.send(procActor, MkDocumentFromTokensCmd(toks, false))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(2)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

  it should "make document from tokens, keep text, add extra word spacing" in {
    val probe = TestProbe()
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val text = toks.map(t => t.mkString("  ")).mkString("   ") // spacing: tok=2, sent=3
    probe.send(procActor, MkDocumentFromTokensCmd(toks, true, 3, 2))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(2)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(text))
  }


  // preprocessText
  it should "preprocess text from zero-length text" in {
    val probe = TestProbe()
    val text = ""
    probe.send(procActor, PreprocessTextCmd(text))
    val reply = probe.expectMsgClass(timeout, classOf[TextMsg])
    (reply.text) must equal(text)
  }

  it should "preprocess simple text" in {
    val probe = TestProbe()
    val text = "Testing is performed."
    probe.send(procActor, PreprocessTextCmd(text))
    val reply = probe.expectMsgClass(timeout, classOf[TextMsg])
    (reply.text) must equal(text)
  }

  // preprocessSentences
  it should "preprocess sentences" in {
    val probe = TestProbe()
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    probe.send(procActor, PreprocessSentencesCmd(sents))
    val reply = probe.expectMsgClass(timeout, classOf[SentencesMsg])
    (reply.sentences.size) must equal(3)
    reply.sentences.zipWithIndex.foreach { case(replySent, ndx) =>
      (replySent) must equal(sents(ndx))
    }
  }

  // preprocessTokens
  it should "preprocess tokens" in {
    val probe = TestProbe()
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    probe.send(procActor, PreprocessTokensCmd(toks))
    val reply = probe.expectMsgClass(timeout, classOf[TokensMsg])
    (reply.tokens.size) must equal(2)
    (reply.tokens.flatten) must equal(toks.flatten)
  }


  // tagPartsOfSpeech
  it should "tagPartsOfSpeech in a small document" in {
    val probe = TestProbe()
    val doc1 = processor.mkDocument("This is a document with a single sentence.")
    probe.send(procActor, TagPartsOfSpeechCmd(doc1))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    val sentences = reply.doc.sentences
    (sentences.size) must equal(1)
    (sentences(0).tags) must not be (empty)
  }

  // lemmatize
  it should "lemmatize a small document" in {
    val probe = TestProbe()
    val doc1 = processor.mkDocument("Children like smaller documents with smaller sentences.")
    probe.send(procActor, TagPartsOfSpeechCmd(doc1))
    var reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    probe.send(procActor, LemmatizeCmd(doc1))
    reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    val sentences = reply.doc.sentences
    (sentences.size) must equal(1)
    (sentences(0).lemmas) must not be (empty)
  }

  // recognizeNamedEntities
  it should "recognize named entities in a small document" in {
    val probe = TestProbe()
    val doc1 = processor.mkDocument(
      "On 6/8/2017, I sent a file containing some C# code to none@nowhere.com.")
    probe.send(procActor, TagPartsOfSpeechCmd(doc1))
    var reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    probe.send(procActor, LemmatizeCmd(doc1))
    reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    probe.send(procActor, RecognizeNamedEntitiesCmd(doc1))
    reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    val sentences = reply.doc.sentences
    (sentences.size) must equal(1)
    (sentences(0).entities) must not be (empty)
  }

  // parse
  it should "parse a small document" in {
    val probe = TestProbe()
    val doc3 = processor.mkDocument(
"""This document has multiple sentences. Each should be processed by the processor.
   A Reach document should be returned.""")
    probe.send(procActor, ParseCmd(doc3))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    val sentences = reply.doc.sentences
    (sentences.size) must equal(3)
    (sentences(0).syntacticTree) must not be (empty)
    (sentences(1).syntacticTree) must not be (empty)
    (sentences(2).syntacticTree) must not be (empty)
  }

  // chunking
  it should "chunk a small document" in {
    val probe = TestProbe()
    val doc2 = processor.mkDocument(
      "Each document can contain many sentences. This document has two sentences.")
    probe.send(procActor, TagPartsOfSpeechCmd(doc2))
    var reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    probe.send(procActor, ChunkingCmd(doc2))
    reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    val sentences = reply.doc.sentences
    (sentences.size) must equal(2)
    (sentences(0).chunks) must not be (empty)
    (sentences(1).chunks) must not be (empty)
  }

  // resolveCoreference
  it should "probably ignore coreference in a small document" in {
    val probe = TestProbe()
    val doc1 = processor.mkDocument("ASPP2 is common, it is well known, and BEF sumoylates it.")
    probe.send(procActor, TagPartsOfSpeechCmd(doc1))
    var reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    probe.send(procActor, LemmatizeCmd(doc1))
    reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    probe.send(procActor, RecognizeNamedEntitiesCmd(doc1))
    reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    probe.send(procActor, ParseCmd(doc1))
    reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    probe.send(procActor, ResolveCoreferenceCmd(doc1))
    reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    val sentences = reply.doc.sentences
    (sentences.size) must equal(1)
    if (usesCoreference)
      (reply.doc.coreferenceChains) must not be (empty)
    else
      (reply.doc.coreferenceChains) must be (empty)
  }

  // discourse
  it should "parse discourse in a small document" in {
    val probe = TestProbe()
    val doc1 = processor.mkDocument(
      "Despite what he said, this is a simple document containing small sentences.")
    probe.send(procActor, TagPartsOfSpeechCmd(doc1))
    var reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    probe.send(procActor, LemmatizeCmd(doc1))
    reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    probe.send(procActor, ParseCmd(doc1))
    reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    probe.send(procActor, DiscourseCmd(doc1))
    reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    // NOTE: following fails if the correct Processor type is not used:
    // (reply.doc.discourseTree) must not be (empty)
  }


  // annotate(text)
  it should "annotate text, default keep" in {
    val probe = TestProbe()
    val text = "This is a document with a single sentence."
    probe.send(procActor, AnnotateTextCmd(text))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(1)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

  it should "annotate text, keep text" in {
    val probe = TestProbe()
    val text = "This is single sentence test."
    probe.send(procActor, AnnotateTextCmd(text, true)) // explicit keep
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(1)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(text))
  }

  it should "annotate text, discard text" in {
    val probe = TestProbe()
    val text = "This is a document with a single sentence."
    probe.send(procActor, AnnotateTextCmd(text, false)) // explicit discard
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(1)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

  // annotateFromSentences
  it should "annotate sentences, default keep" in {
    val probe = TestProbe()
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    probe.send(procActor, AnnotateFromSentencesCmd(sents))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(3)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

  it should "annotate sentences, keep text" in {
    val probe = TestProbe()
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    probe.send(procActor, AnnotateFromSentencesCmd(sents, true)) // explicit keep
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(3)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(sents.mkString(" ")))
  }

  it should "annotate sentences, discard text" in {
    val probe = TestProbe()
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    probe.send(procActor, AnnotateFromSentencesCmd(sents, false)) // explicit discard
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(3)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }


  // annotateFromTokens
  it should "annotate tokens, default keep" in {
    val probe = TestProbe()
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    probe.send(procActor, AnnotateFromTokensCmd(toks))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(2)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

  it should "annotate tokens, keep text" in {
    val probe = TestProbe()
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val text = toks.map(t => t.mkString(" ")).mkString(" ")  // spacing: tok=1, sent=1
    probe.send(procActor, AnnotateFromTokensCmd(toks, true)) // explicit keep
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(2)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(text))
  }

  it should "annotate tokens, discard text" in {
    val probe = TestProbe()
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    probe.send(procActor, AnnotateFromTokensCmd(toks, false)) // explicit discard
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(2)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }


  // annotate(Document)
  it should "annotate zero-length Document, default keep" in {
    val probe = TestProbe()
    val doc0 = processor.mkDocument("")
    probe.send(procActor, AnnotateCmd(doc0))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(0)
    (reply.doc.text).isDefined must be (false)
  }

  it should "annotate single sentence Document, no text to propagate" in {
    val probe = TestProbe()
    val text = "This is a document with a single sentence."
    val doc = processor.mkDocument(text)    // doc w/o kept text
    probe.send(procActor, AnnotateCmd(doc))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(1)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

  it should "annotate single sentence Document, keep text" in {
    val probe = TestProbe()
    val text = "This is a document with a single sentence."
    val doc = processor.mkDocument(text, true) // doc with text kept
    probe.send(procActor, AnnotateCmd(doc))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(1)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(text))
  }

  it should "annotate single sentence Document, discard text" in {
    val probe = TestProbe()
    val text = "This is a document with a single sentence."
    val doc = processor.mkDocument(text, false) // doc w/o kept text
    probe.send(procActor, AnnotateCmd(doc))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(1)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }


  it should "annotate multi-sentence Document, no text to propagate" in {
    val probe = TestProbe()
    val text = "This document has multiple sentences. Each should be processed by the processor. A Reach document should be returned."
    val doc = processor.mkDocument(text)    // doc w/o kept text
    probe.send(procActor, AnnotateCmd(doc))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(3)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

  it should "annotate multi-sentence Document, keep text" in {
    val probe = TestProbe()
    val text = "This document has multiple sentences. Each should be processed by the processor. A Reach document should be returned."
    val doc = processor.mkDocument(text, true) // doc with text kept
    probe.send(procActor, AnnotateCmd(doc))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(3)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(text))
  }

  it should "annotate multi-sentence Document, discard text" in {
    val probe = TestProbe()
    val text = "This document has multiple sentences. Each should be processed by the processor. A Reach document should be returned."
    val doc = processor.mkDocument(text)    // doc w/o kept text
    probe.send(procActor, AnnotateCmd(doc))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(3)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

}
