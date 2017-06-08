package org.clulab.coserver

import scala.concurrent.duration._

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.actor.{ ActorSystem, Props, Actor }
import akka.event.Logging
import akka.testkit.{ TestKit, TestActorRef, TestProbe, ImplicitSender }
import org.scalatest.{ BeforeAndAfterAll, FlatSpecLike, MustMatchers }

import org.clulab.processors._
import org.clulab.processors.corenlp._

import CoreServerMessages._

/**
  * Unit tests of the ProcessorActor class.
  *   Written by: Tom Hicks. 6/6/2016.
  *   Last Modified: Check number of sentences on result document.
  */
class TestProcessorActor extends TestKit(ActorSystem("test-proc-actor"))
    with FlatSpecLike
    with ImplicitSender
    with BeforeAndAfterAll
    with MustMatchers
{
  val processor: Processor = new CoreNLPProcessor()
  val procActor = system.actorOf(ProcessorActor.props(processor))
  val doc0 = processor.mkDocument("")
  val doc1 = processor.mkDocument("This is a document with a single sentence.")
  val doc3 = processor.mkDocument(
    """This document has multiple sentences. Each should be processed by the processor.
       A Reach document should be returned.""")

  val timeout: FiniteDuration = 2.minutes

  override def afterAll = {
    TestKit.shutdownActorSystem(system)
  }

  "ProcessorActor" should "round-trip zero-length document" in {
    val sender = TestProbe()
    sender.send(procActor, AnnotateCmd(doc0))
    val state = sender.expectMsgClass(timeout, classOf[DocumentMsg])
    (state.doc.sentences.size) must equal(0)
  }

  it should "round-trip single sentence document" in {
    val sender = TestProbe()
    sender.send(procActor, AnnotateCmd(doc1))
    val state = sender.expectMsgClass(timeout, classOf[DocumentMsg])
    (state.doc.sentences.size) must equal(1)
  }

  it should "round-trip multi-sentence document" in {
    val sender = TestProbe()
    sender.send(procActor, AnnotateCmd(doc3))
    val state = sender.expectMsgClass(timeout, classOf[DocumentMsg])
    (state.doc.sentences.size) must equal(3)
  }

}
