package org.clulab.processors.coserver

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

import org.scalatest.{ BeforeAndAfterAll, Matchers, FlatSpecLike }

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.actor._
import akka.routing._
import akka.pattern.ask
import akka.testkit.{ TestKit, TestActorRef, TestProbe, ImplicitSender }
import akka.util.Timeout

import org.clulab.processors.Document
import org.clulab.processors.coshare.ProcessorCoreMessages._

/**
  * Tests of the ProcessorCoreServer.
  *   Written by: Tom Hicks. 6/14/2017.
  *   Last Modified: Update for implementation of processor annotator trait only.
  */
class TestProcessorCoreServer extends TestKit(ActorSystem("testProcCoreServer"))
    with FlatSpecLike
    with BeforeAndAfterAll
    with Matchers
    with LazyLogging
{
  val config = ConfigFactory.load().getConfig("ProcessorCoreServer")

  // shutdown the actor system when done testing
  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  // create a processor core server instance
  val pcs = ProcessorCoreServer.instance
  logger.debug(s"ProcessorCoreServer.instance=${pcs}")

  // get a reference to the pooled router from the core server instance
  val router = pcs.router
  logger.debug(s"ProcessorCoreServer.router=${router}")

  // simulate blocking RPC: finite duration is required so make it long
  implicit val timeout = Timeout(8 hours)  // time limit to return Future from call

  /** Send the given message to the server and block until response comes back. */
  def callServer (request: ProcessorCoreCommand): ProcessorCoreReply = {
    val response = router ? request         // call returning Future
    Await.result(response, Duration.Inf).asInstanceOf[ProcessorCoreReply]
  }

  "ProcessorCoreServer" should "the pooled router should not be null" in {
    (router) should not be (null)
  }

  /** Core Server is alive, now give it a simple test. */
  it should "annotate single sentence, keep text" in {
    val text = "This is single sentence test."
    val reply = callServer(AnnotateTextCmd(text, true)) // keep text
    val doc = reply.asInstanceOf[DocumentMsg].doc
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

}
