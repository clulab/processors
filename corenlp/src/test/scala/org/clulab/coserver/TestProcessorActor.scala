package org.clulab.coserver

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.actor.{ ActorSystem, Props, Actor }
import akka.event.Logging
import akka.testkit.{ TestKit, TestActorRef, TestProbe, ImplicitSender }
import org.scalatest.{ BeforeAndAfterAll, FlatSpecLike, MustMatchers }

import org.clulab.processors._
import org.clulab.processors.corenlp._

import CoreProcessorCommands._
import CoreProcessorReplies._

/**
  * Unit tests of the ProcessorActor class.
  *   Written by: Tom Hicks. 6/6/2016.
  *   Last Modified: Initial creation: tests of stubs methods only (to test infrastructure).
  */
class TestProcessorActor extends TestKit(ActorSystem("test-proc-actor"))
    with FlatSpecLike
    with ImplicitSender
    with BeforeAndAfterAll
    with MustMatchers
{
  val core: Processor = new CoreNLPProcessor()

  override def afterAll = {
    TestKit.shutdownActorSystem(system)
  }

  "ProcessorActor" should "round-trip zero-length message" in {
    val procActor = system.actorOf(ProcessorActor.props(core))
    val sender = TestProbe()
    sender.send(procActor, AnnotateCmd(""))
    val state = sender.expectMsgType[TextMsg]
    state must equal(TextMsg("Length 0"))
  }

  "ProcessorActor" should "round-trip non-empty message" in {
    val procActor = system.actorOf(ProcessorActor.props(core))
    val sender = TestProbe()
    sender.send(procActor, AnnotateCmd("Test test test"))
    val state = sender.expectMsgType[TextMsg]
    state must equal(TextMsg("Length 14"))
  }

}
