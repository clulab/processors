package org.clulab.processors.coclient

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import org.scalatest.{ Matchers, FlatSpec }

import org.clulab.processors.Document
import org.clulab.processors.coshare.ProcessorCoreMessages._
import org.clulab.utils.StringUtils

/**
  * Tests of the ProcessorCoreClient.
  *   Written by: Tom Hicks. 6/20/2017.
  *   Last Modified: Update for implementation of processor annotator trait only.
  */
class TestProcessorCoreClient extends FlatSpec with Matchers with LazyLogging {

  // load application configuration from the configuration file
  val config = ConfigFactory.load().getConfig("ProcessorCoreClient")
  logger.debug(s"(TestProcessorCoreClient): config=${config}")

  // create a processor core server instance
  val client = new ProcessorCoreClient(config)
  logger.debug(s"(TestProcessorCoreClient): client=${client}")

  def logDoc (doc: Document): Unit = {
    val id = doc.id.getOrElse("")
    val ssize = doc.sentences.size
    val coref = doc.coreferenceChains.getOrElse(None)
    val discT = doc.discourseTree.getOrElse(None)
    val text = doc.text.getOrElse("")
    val clazz = doc.getClass.getName
    logger.error(s"DD[${clazz}](id=${id}, sents.size=${ssize}, coref=${coref}, discT=${discT}, text=${text})")
  }


  "ProcessorCoreClient" should "not be null" in {
    (client) should not be (null)
  }

  it should "get reference to the pooled router" in {
    val router = client.router
    logger.debug(s"(TestProcessorCoreClient): router=${router}")
    (router) should not be (null)
  }

  // ErrorTest -- needs special errorTest method to be defined in client
  // it should "throw exception for error test" in {
  //   val ex = the [RuntimeException] thrownBy client.errorTest
  //   (ex.getMessage) should equal ("This is a fake error from the ErrorTest command.")
  // }

  // annotate(text)
  it should "annotate text, default keep" in {
    logger.debug(s"(TestProcessorCoreClient): annotate text, default keep")
    val text = "This is a document with a single sentence."
    val doc = client.annotate(text)
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "annotate text, keep text" in {
    logger.debug(s"(TestProcessorCoreClient): annotate text, keep text")
    val text = "This is single sentence test."
    val doc = client.annotate(text, true)   // explicit keep
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "annotate text, discard text" in {
    logger.debug(s"(TestProcessorCoreClient): annotate text, discard text")
    val text = "This is a document with a single sentence."
    val doc = client.annotate(text, false)  // explicit discard
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  // annotateFromSentences
  it should "annotate sentences, default keep" in {
    logger.debug(s"(TestProcessorCoreClient): annotate sentences, default keep")
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val doc = client.annotateFromSentences(sents)
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "annotate sentences, keep text" in {
    logger.debug(s"(TestProcessorCoreClient): annotate sentences, keep text")
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val doc = client.annotateFromSentences(sents, true) // explicit keep
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(sents.mkString(" ")))
  }

  it should "annotate sentences, discard text" in {
    logger.debug(s"(TestProcessorCoreClient): annotate sentences, discard text")
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val doc = client.annotateFromSentences(sents, false) // explicit discard
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  // annotateFromTokens
  it should "annotate tokens, default keep" in {
    logger.debug(s"(TestProcessorCoreClient): annotate tokens, default keep")
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val doc = client.annotateFromTokens(toks)
    (doc) should not be (null)
    (doc.sentences.size) should equal(2)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "annotate tokens, keep text" in {
    logger.debug(s"(TestProcessorCoreClient): annotate tokens, keep text")
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val text = toks.map(t => t.mkString(" ")).mkString(" ")  // spacing: tok=1, sent=1
    val doc = client.annotateFromTokens(toks, true)       // explicit keep
    (doc) should not be (null)
    (doc.sentences.size) should equal(2)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "annotate tokens, discard text" in {
    logger.debug(s"(TestProcessorCoreClient): annotate tokens, discard text")
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val doc = client.annotateFromTokens(toks, false) // explicit discard
    (doc) should not be (null)
    (doc.sentences.size) should equal(2)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

}
