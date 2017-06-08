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
  *   Last Modified: Update for message consolidation.
  */
class TestProcessorActor extends TestKit(ActorSystem("test-proc-actor"))
    with FlatSpecLike
    with ImplicitSender
    with BeforeAndAfterAll
    with MustMatchers
{
  val core: Processor = new CoreNLPProcessor()
  val procActor = system.actorOf(ProcessorActor.props(core))

  override def afterAll = {
    TestKit.shutdownActorSystem(system)
  }

  "ProcessorActor" should "round-trip zero-length message" in {
    val sender = TestProbe()
    sender.send(procActor, AnnotateCmd(""))
    val state = sender.expectMsgClass(1.minute, classOf[DocumentMsg])
//    state must equal(TextMsg("Length 0"))
  }

  it should "round-trip non-empty message" in {
    val sender = TestProbe()
    sender.send(procActor, AnnotateCmd("This is a sentence to annotate with annotations"))
    val state = sender.expectMsgClass(1.minute, classOf[DocumentMsg])
//    state must equal(TextMsg("Length 14"))
  }

}
