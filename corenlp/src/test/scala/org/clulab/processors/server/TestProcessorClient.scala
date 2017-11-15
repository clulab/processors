package org.clulab.processors.client

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

import org.scalatest.{ BeforeAndAfterAll, Matchers, FlatSpecLike }

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.testkit.{ TestKit, TestActorRef, TestProbe, ImplicitSender }

import org.clulab.processors.Document
import org.clulab.processors.server.ProcessorServer
import org.clulab.processors.csshare.ProcessorCSMessages._
import org.clulab.utils.StringUtils

/**
  * Tests of the ProcessorClient. Even though this is a test of the *client*,
  * it must be located in the server (corenlp) subproject because of the one-way dependency
  * between the server code and the client (main) subproject.
  *   Written by: Tom Hicks. 6/20/2017.
  *   Last Modified: Restore preprocess* tests.
  */
class TestProcessorClient extends FlatSpecLike
    with Matchers
    with BeforeAndAfterAll
    with LazyLogging
{
  // load server configuration from the configuration file, specify a BioNLP processor
  val sConfig = ConfigFactory.load().getConfig("ProcessorServer")
  val pcsConfig = sConfig.withValue("server.processor.type", ConfigValueFactory.fromAnyRef("bio"))
  logger.debug(s"(TestProcessorClient): pcsConfig=${pcsConfig}")

  // fire up a server to run these tests against
  val pcs = new ProcessorServer(pcsConfig)

  // load application configuration from the configuration file
  val config = ConfigFactory.load().getConfig("ProcessorClient")
  logger.debug(s"(TestProcessorClient): config=${config}")

  // create a processor server instance
  val client = new ProcessorClient(config)
  logger.debug(s"(TestProcessorClient): client=${client}")

  // shutdown the actor systems when done testing
  override def afterAll {
    TestKit.shutdownActorSystem(client.system)
    TestKit.shutdownActorSystem(pcs.system)
  }


  def logDoc (doc: Document): Unit = {
    val id = doc.id.getOrElse("")
    val ssize = doc.sentences.size
    val coref = doc.coreferenceChains.getOrElse(None)
    val discT = doc.discourseTree.getOrElse(None)
    val text = doc.text.getOrElse("")
    val clazz = doc.getClass.getName
    logger.error(s"DD[${clazz}](id=${id}, sents.size=${ssize}, coref=${coref}, discT=${discT}, text=${text})")
  }


  "ProcessorClient" should "not be null" in {
    (client) should not be (null)
  }

  it should "get reference to the pooled router" in {
    val router = client.router
    logger.debug(s"(TestProcessorClient): router=${router}")
    (router) should not be (null)
  }

  // ErrorTest -- needs special errorTest method to be defined in client
  // it should "throw exception for error test" in {
  //   val ex = the [RuntimeException] thrownBy client.errorTest
  //   (ex.getMessage) should equal ("This is a fake error from the ErrorTest command.")
  // }

  // annotate(text)
  it should "annotate text, default keep" in {
    logger.debug(s"(TestProcessorClient): annotate text, default keep")
    val text = "This is a document with a single sentence."
    val doc = client.annotate(text)
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "annotate text, keep text" in {
    logger.debug(s"(TestProcessorClient): annotate text, keep text")
    val text = "This is single sentence test."
    val doc = client.annotate(text, true)   // explicit keep
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "annotate text, discard text" in {
    logger.debug(s"(TestProcessorClient): annotate text, discard text")
    val text = "This is a document with a single sentence."
    val doc = client.annotate(text, false)  // explicit discard
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  // annotateFromSentences
  it should "annotate sentences, default keep" in {
    logger.debug(s"(TestProcessorClient): annotate sentences, default keep")
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val doc = client.annotateFromSentences(sents)
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "annotate sentences, keep text" in {
    logger.debug(s"(TestProcessorClient): annotate sentences, keep text")
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val doc = client.annotateFromSentences(sents, true) // explicit keep
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(sents.mkString(" ")))
  }

  it should "annotate sentences, discard text" in {
    logger.debug(s"(TestProcessorClient): annotate sentences, discard text")
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val doc = client.annotateFromSentences(sents, false) // explicit discard
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  // annotateFromTokens
  it should "annotate tokens, default keep" in {
    logger.debug(s"(TestProcessorClient): annotate tokens, default keep")
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val doc = client.annotateFromTokens(toks)
    (doc) should not be (null)
    (doc.sentences.size) should equal(2)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "annotate tokens, keep text" in {
    logger.debug(s"(TestProcessorClient): annotate tokens, keep text")
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val text = toks.map(t => t.mkString(" ")).mkString(" ")  // spacing: tok=1, sent=1
    val doc = client.annotateFromTokens(toks, true)       // explicit keep
    (doc) should not be (null)
    (doc.sentences.size) should equal(2)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "annotate tokens, discard text" in {
    logger.debug(s"(TestProcessorClient): annotate tokens, discard text")
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val doc = client.annotateFromTokens(toks, false) // explicit discard
    (doc) should not be (null)
    (doc.sentences.size) should equal(2)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }


  // preprocessText
  it should "preprocess text from zero-length text" in {
    logger.debug(s"(TestProcessorCoreClient): preprocess text from zero-length text")
    val text = ""
    val reply = client.preprocessText(text)
    (reply) should not be (null)
    (reply) should equal(text)
  }

  it should "preprocess simple text" in {
    logger.debug(s"(TestProcessorCoreClient): preprocess simple text")
    val text = "Testing is performed."
    val reply = client.preprocessText(text)
    (reply) should not be (null)
    (reply) should equal(text)
  }

  // preprocessSentences
  it should "preprocess sentences" in {
    logger.debug(s"(TestProcessorCoreClient): preprocess sentences")
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val reply = client.preprocessSentences(sents)
    (reply) should not be (null)
    (reply.size) should equal(3)
    reply.zipWithIndex.foreach { case(sent, ndx) =>
      (sent) should equal(sents(ndx))
    }
  }

  // preprocessTokens
  it should "preprocess tokens" in {
    logger.debug(s"(TestProcessorCoreClient): preprocess tokens")
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val reply = client.preprocessTokens(toks)
    (reply.size) should equal(2)
    (reply.flatten) should equal(toks.flatten)
  }

}
