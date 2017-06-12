package org.clulab.processors.coserver

import scala.concurrent.duration._

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.actor.{ ActorSystem, Props, Actor }
import akka.event.Logging
import akka.testkit.{ TestKit, TestActorRef, TestProbe, ImplicitSender }
import org.scalatest.{ BeforeAndAfterAll, FlatSpecLike, MustMatchers }

import org.clulab.processors._
import org.clulab.processors.bionlp._
import org.clulab.processors.corenlp._
import org.clulab.processors.fastnlp._
import org.clulab.processors.shallownlp._

import ProcessorCoreServerMessages._

/**
  * Unit tests of the ProcessorActor class.
  *   Written by: Tom Hicks. 6/6/2016.
  *   Last Modified: Add tests for make document methods.
  */
class TestProcessorActor extends TestKit(ActorSystem("test-proc-actor"))
    with FlatSpecLike
    with ImplicitSender
    with BeforeAndAfterAll
    with MustMatchers
{
  val config = ConfigFactory.load().getConfig("ProcessorCoreService")

  // create the Processor engine specified by the configuration and used by this server
  val processor: Processor = {
    val proc = config.getString("server.processor")
    proc.toLowerCase match {
      case "bio" => new BioNLPProcessor(removeFigTabReferences = true)
      case "core" => new CoreNLPProcessor()
      case "fast" => new FastNLPProcessor(useMalt = false)
      case "fastbio" => new FastBioNLPProcessor(removeFigTabReferences = true)
      case _ => new ShallowNLPProcessor()
    }
  }

  val procActor = system.actorOf(ProcessorActor.props(processor))
  val timeout: FiniteDuration = 1.minutes

  override def afterAll = {
    TestKit.shutdownActorSystem(system)
  }

  // mkDocument
  "ProcessorActor" should "make document from zero-length text, keep text" in {
    val probe = TestProbe()
    val text = ""
    probe.send(procActor, MkDocumentCmd(text, true))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(0)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(text))
  }

  it should "make document from simple text, keep text" in {
    val probe = TestProbe()
    val text = "This is a test."
    probe.send(procActor, MkDocumentCmd(text, true))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(1)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(text))
  }

  it should "make document from simple text, discard text" in {
    val probe = TestProbe()
    val text = "This is a test."
    probe.send(procActor, MkDocumentCmd(text, false))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(1)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

  // mkDocumentFromSentences
  it should "make document from sentences, keep text" in {
    val probe = TestProbe()
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    probe.send(procActor, MkDocumentFromSentencesCmd(sents, true))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(3)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(sents.mkString(" "))) // spacing: sent=1
  }

  it should "make document from sentences, discard text" in {
    val probe = TestProbe()
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    probe.send(procActor, MkDocumentFromSentencesCmd(sents, false))
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


  // annotate
  it should "annotate zero-length document" in {
    val probe = TestProbe()
    val doc0 = processor.mkDocument("")
    probe.send(procActor, AnnotateCmd(doc0))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(0)
  }

  it should "annotate single sentence document" in {
    val probe = TestProbe()
    val doc1 = processor.mkDocument("This is a document with a single sentence.")
    probe.send(procActor, AnnotateCmd(doc1))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(1)
  }

  it should "annotate multi-sentence document" in {
    val probe = TestProbe()
    val doc3 = processor.mkDocument(
"""This document has multiple sentences. Each should be processed by the processor.
   A Reach document should be returned.""")
    probe.send(procActor, AnnotateCmd(doc3))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(3)
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

  // label semantic roles
  it should "label semantic roles in a small document" in {
    val probe = TestProbe()
    val doc1 = processor.mkDocument("Mary told John that Bill hit Sue.")
    probe.send(procActor, LabelSemanticRolesCmd(doc1))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    val sentences = reply.doc.sentences
    (sentences.size) must equal(1)
    // NOTE: following fails: no semantic labeling code in Processors yet.
    // (sentences(0).stanfordBasicDependencies) must not be (empty)
  }

  // resolveCoreference
  it should "resolve coreference in a small document" in {
    val probe = TestProbe()
    val doc1 = processor.mkDocument("This is a document and it has one sentence and we like it.")
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
    (reply.doc.coreferenceChains) must not be (empty)
  }

  // discourse
  // it should "parse discourse in a small document" in {
  //   val probe = TestProbe()
  //   val doc1 = processor.mkDocument(
  //     "Despite what he said, this is a simple document containing small sentences.")
  //   probe.send(procActor, TagPartsOfSpeechCmd(doc1))
  //   var reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
  //   probe.send(procActor, LemmatizeCmd(doc1))
  //   reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
  //   probe.send(procActor, ParseCmd(doc1))
  //   reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
  //   probe.send(procActor, DiscourseCmd(doc1))
  //   reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
  //   (reply.doc.discourseTree) must not be (empty)
  // }

}
