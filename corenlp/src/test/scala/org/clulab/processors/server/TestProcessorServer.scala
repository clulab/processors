package org.clulab.processors.server

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
import org.clulab.processors.csshare.ProcessorCSMessages._

/**
  * Tests of the ProcessorServer.
  *   Written by: Tom Hicks. 6/14/2017.
  *   Last Modified: Use Processors serialization/unserialization for Documents.
  */
class TestProcessorServer extends TestKit(ActorSystem("testProcServer"))
    with FlatSpecLike
    with BeforeAndAfterAll
    with Matchers
    with LazyLogging
{
  val config = ConfigFactory.load().getConfig("ProcessorServer")

  // shutdown the actor system when done testing
  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  // create a processor server instance
  val pcs = ProcessorServer.instance
  logger.debug(s"ProcessorServer.instance=${pcs}")

  // get a reference to the pooled router from the server instance
  val router = pcs.router
  logger.debug(s"ProcessorServer.router=${router}")

  // simulate blocking RPC: finite duration is required so make it long
  implicit val timeout = Timeout(8 hours)  // time limit to return Future from call

  /** Send the given message to the server and block until response comes back. */
  def callServer (request: ProcessorCSCommand): ProcessorCSReply = {
    val response = router ? request         // call returning Future
    Await.result(response, Duration.Inf).asInstanceOf[ProcessorCSReply]
  }

  "ProcessorServer" should "the pooled router should not be null" in {
    (router) should not be (null)
  }

  /** Server is alive, now give it a simple test. */
  it should "preprocess single sentence" in {
    val input = "This is single sentence test."
    val reply = callServer(PreprocessTextCmd(input))
    (reply) should not be (null)
    val text = reply.asInstanceOf[TextMsg].text
    (text) should not be (empty)
    (text) should equal(input)
  }

}
