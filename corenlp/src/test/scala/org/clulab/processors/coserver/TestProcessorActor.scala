package org.clulab.processors.coserver

import scala.concurrent.duration._

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.actor.{ ActorSystem, Props, Actor }
import akka.event.Logging
import akka.testkit.{ TestKit, TestActorRef, TestProbe, ImplicitSender }
import org.scalatest.{ BeforeAndAfterAll, FlatSpecLike, MustMatchers }

import org.clulab.processors._
// import org.clulab.processors.bionlp._
import org.clulab.processors.corenlp._
import org.clulab.processors.shallownlp._

import ProcessorCoreServerMessages._

/**
  * Unit tests of the ProcessorActor class.
  *   Written by: Tom Hicks. 6/6/2016.
  *   Last Modified: Move coserver package.
  */
class TestProcessorActor extends TestKit(ActorSystem("test-proc-actor"))
    with FlatSpecLike
    with ImplicitSender
    with BeforeAndAfterAll
    with MustMatchers
{
  val processor:Processor = new CoreNLPProcessor()
  // val processor:Processor = new BioNLPProcessor (
  //   withCRFNER = false,
  //   withRuleNER = false,
  //   withDiscourse = ShallowNLPProcessor.WITH_DISCOURSE
  // )
  val procActor = system.actorOf(ProcessorActor.props(processor))
  val timeout: FiniteDuration = 1.minutes

  override def afterAll = {
    TestKit.shutdownActorSystem(system)
  }

  "ProcessorActor" should "annotate zero-length document" in {
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

  it should "tagPartsOfSpeech in a small document" in {
    val probe = TestProbe()
    val doc1 = processor.mkDocument("This is a document with a single sentence.")
    probe.send(procActor, TagPartsOfSpeechCmd(doc1))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    val sentences = reply.doc.sentences
    (sentences.size) must equal(1)
    (sentences(0).tags) must not be (empty)
  }

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

  it should "label semantic roles in a small document" in {
    val probe = TestProbe()
    val doc1 = processor.mkDocument("Mary told John that Bill hit Sue.")
    probe.send(procActor, LabelSemanticRolesCmd(doc1))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    val sentences = reply.doc.sentences
    (sentences.size) must equal(1)
    // NOTE: following fails: no semantic labeling code found in Processors
    // (sentences(0).stanfordBasicDependencies) must not be (empty)
  }

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
