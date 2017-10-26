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
  *   Last Modified: Move processing chain for side-effecting methods to server side.
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

  // mkDocument
  it should "make document from zero-length text, keep text" in {
    logger.debug(s"(TestProcessorCoreClient): make document from zero-length text, keep text")
    val text = ""
    val doc = client.mkDocument(text, true) // keep text
    (doc) should not be (null)
    (doc.sentences.size) should equal(0)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "make document from simple text, default keep" in {
    logger.debug(s"(TestProcessorCoreClient): make document from simple text, default keep")
    val text = "This is a test."
    val doc = client.mkDocument(text) // default keep
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "make document from simple text, keep text" in {
    logger.debug(s"(TestProcessorCoreClient): make document from simple text, keep text")
    val text = "This is a test."
    val doc = client.mkDocument(text, true) // keep text
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "make document from simple text, discard text" in {
    logger.debug(s"(TestProcessorCoreClient): make document from simple text, discard text")
    val text = "This is a test."
    val doc = client.mkDocument(text, false) // discard text
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  // mkDocumentFromSentences
  it should "make document from sentences, keep text" in {
    logger.debug(s"(TestProcessorCoreClient): make document from sentences, keep text")
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val doc = client.mkDocumentFromSentences(sents, true) // keep text
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(sents.mkString(" "))) // spacing: sent=1
  }

  it should "make document from sentences, discard text" in {
    logger.debug(s"(TestProcessorCoreClient): make document from sentences, discard text")
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val doc = client.mkDocumentFromSentences(sents, false) // discard text
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "make document from sentences, keep text, add extra spacing" in {
    logger.debug(s"(TestProcessorCoreClient): make document from sentences, keep text, add extra spacing")
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val doc = client.mkDocumentFromSentences(sents, true, 3)
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(sents.mkString("   "))) // spacing: sent=3
  }

  // mkDocumentFromTokens
  it should "make document from tokens, keep text" in {
    logger.debug(s"(TestProcessorCoreClient): make document from tokens, keep text")
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val text = toks.map(t => t.mkString(" ")).mkString(" ") // spacing: tok=1, sent=1
    val doc = client.mkDocumentFromTokens(toks, true)
    (doc) should not be (null)
    (doc.sentences.size) should equal(2)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "make document from tokens, discard text" in {
    logger.debug(s"(TestProcessorCoreClient): make document from tokens, discard text")
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val doc = client.mkDocumentFromTokens(toks, false)
    (doc) should not be (null)
    (doc.sentences.size) should equal(2)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "make document from tokens, keep text, add extra word spacing" in {
    logger.debug(s"(TestProcessorCoreClient): make document from tokens, keep text, add extra word spacing")
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val text = toks.map(t => t.mkString("  ")).mkString("   ") // spacing: tok=2, sent=3
    val doc = client.mkDocumentFromTokens(toks, true, 3, 2)
    (doc) should not be (null)
    (doc.sentences.size) should equal(2)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
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


  // tagPartsOfSpeech
  it should "tagPartsOfSpeech in a small document" in {
    logger.debug(s"(TestProcessorCoreClient): tagPartsOfSpeech in a small document")
    val text = "This is a document with a single sentence."
    val doc = client.tagPartsOfSpeech(client.mkDocument(text, true))
    val sentences = doc.sentences
    (sentences.size) should equal(1)
    (sentences(0).tags) should not be (empty)
  }

  // lemmatize
  it should "lemmatize a small document" in {
    logger.debug(s"(TestProcessorCoreClient): lemmatize in a small document")
    val text = "Children like smaller documents with smaller sentences."
    val doc = client.lemmatize(client.mkDocument(text, true))
    val sentences = doc.sentences
    (sentences.size) should equal(1)
    (sentences(0).lemmas) should not be (empty)
  }

  // recognizeNamedEntities
  it should "recognize named entities in a small document" in {
    logger.debug(s"(TestProcessorCoreClient): NER in a small document")
    val text = "On 6/8/2017, I sent a file containing some C# code to none@nowhere.com."
    val doc = client.recognizeNamedEntities(client.mkDocument(text, true))
    val sentences = doc.sentences
    (sentences.size) should equal(1)
    (sentences(0).tags) should not be (empty)
    (sentences(0).lemmas) should not be (empty)
    (sentences(0).entities) should not be (empty)
  }

  // parse
  it should "parse a small document" in {
    logger.debug(s"(TestProcessorCoreClient): parse a small document")
    val text =
"""This document has multiple sentences. Each should be processed by the processor.
   A Document should be returned."""
    val doc = client.parse(client.mkDocument(text, true))
    val sentences = doc.sentences
    (sentences.size) should equal(3)
    (sentences(0).syntacticTree) should not be (empty)
    (sentences(1).syntacticTree) should not be (empty)
    (sentences(2).syntacticTree) should not be (empty)
  }

  // chunking
  it should "chunk a small document" in {
    logger.debug(s"(TestProcessorCoreClient): chunk a small document")
    val text = "Each document can contain many sentences. This document has two sentences."
    val doc = client.chunking(client.mkDocument(text, true))
    val sentences = doc.sentences
    (sentences.size) should equal(2)
    (sentences(0).tags) should not be (empty)
    (sentences(0).chunks) should not be (empty)
    (sentences(1).chunks) should not be (empty)
  }

  // resolveCoreference
  it should "resolve coreference in a small document" in {
    logger.debug(s"(TestProcessorCoreClient): resolve coreference in a small document")
    val text = "This is a document and it has one sentence and we like it."
    val doc = client.resolveCoreference(client.mkDocument(text, true))
    val sentences = doc.sentences
    (sentences.size) should equal(1)
    (doc.coreferenceChains) should be (empty) // BioNLP does not use coreference
  }

  // discourse
  it should "parse discourse in a small document" in {
    logger.debug(s"(TestProcessorCoreClient): parse discourse in a small document")
    val text = "Despite what he said, this is a simple document containing small sentences."
    val doc = client.discourse(client.mkDocument(text, true))
    // NOTE: following fails if the correct Processor type is not used:
    // (doc.discourseTree) should not be (empty)
  }


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

  // annotate(Document)
  it should "annotate zero-length Document, default keep" in {
    logger.debug(s"(TestProcessorCoreClient): annotate zero-length document, default keep")
    val docIn = client.mkDocument("")
    val doc = client.annotate(docIn)
    (doc) should not be (null)
    (doc.sentences.size) should equal(0)
    (doc.text).isDefined should be (false)
  }

  it should "annotate single sentence Document, no text to propagate" in {
    logger.debug(s"(TestProcessorCoreClient): annotate single sentence document, no text")
    val text = "This is a document with a single sentence."
    val docIn = client.mkDocument(text)     // doc w/o kept text
    val doc = client.annotate(docIn)
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "annotate single sentence Document, keep text" in {
    logger.debug(s"(TestProcessorCoreClient): annotate single sentence document, keep text")
    val text = "This is a document with a single sentence."
    val docIn = client.mkDocument(text, true) // doc with text kept
    val doc = client.annotate(docIn)
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "annotate single sentence Document, discard text" in {
    logger.debug(s"(TestProcessorCoreClient): annotate single sentence document, discard text")
    val text = "This is a document with a single sentence."
    val docIn = client.mkDocument(text, false) // doc w/o kept text
    val doc = client.annotate(docIn)
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "annotate multi-sentence Document, no text to propagate" in {
    logger.debug(s"(TestProcessorCoreClient): annotate multi-sentence document, no text")
    val text = "This document has multiple sentences. Each should be processed by the processor. A Document should be returned."
    val docIn = client.mkDocument(text)    // doc w/o kept text
    val doc = client.annotate(docIn)
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "annotate multi-sentence Document, keep text" in {
    logger.debug(s"(TestProcessorCoreClient): annotate multi-sentence document, keep text")
    val text = "This document has multiple sentences. Each should be processed by the processor. A Document should be returned."
    val docIn = client.mkDocument(text, true) // doc with text kept
    val doc = client.annotate(docIn)
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "annotate multi-sentence Document, discard text" in {
    logger.debug(s"(TestProcessorCoreClient): annotate multi-sentence document, discard text")
    val text = "This document has multiple sentences. Each should be processed by the processor. A Document should be returned."
    val docIn = client.mkDocument(text)    // doc w/o kept text
    val doc = client.annotate(docIn)
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

}
