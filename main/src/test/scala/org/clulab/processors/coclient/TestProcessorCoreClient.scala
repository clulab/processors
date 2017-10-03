package org.clulab.reach.coserver

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import org.scalatest.{ Matchers, FlatSpec }

import org.clulab.processors.Document

/**
  * Tests of the ProcessorCoreClient.
  *   Written by: Tom Hicks. 6/20/2017.
  *   Last Modified: Update for use of BioNLP.
  */
class TestProcessorCoreClient extends FlatSpec with Matchers with LazyLogging {

  // load application configuration from the configuration file
  val config = ConfigFactory.load().getConfig("ProcessorCoreClient")
  logger.debug(s"(TestProcessorCoreClient): config=${config}")

  // create a processor core server instance
  val client = new ProcessorCoreClient
  logger.debug(s"(TestProcessorCoreClient): client=${client}")

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
    val text = ""
    val doc = client.mkDocument(text, true) // keep text
    (doc) should not be (null)
    (doc.sentences.size) should equal(0)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "make document from simple text, default keep" in {
    val text = "This is a test."
    val doc = client.mkDocument(text) // default keep
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "make document from simple text, keep text" in {
    val text = "This is a test."
    val doc = client.mkDocument(text, true) // keep text
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "make document from simple text, discard text" in {
    val text = "This is a test."
    val doc = client.mkDocument(text, false) // discard text
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  // mkDocumentFromSentences
  it should "make document from sentences, keep text" in {
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val doc = client.mkDocumentFromSentences(sents, true) // keep text
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(sents.mkString(" "))) // spacing: sent=1
  }

  it should "make document from sentences, discard text" in {
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val doc = client.mkDocumentFromSentences(sents, false) // discard text
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "make document from sentences, keep text, add extra spacing" in {
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val doc = client.mkDocumentFromSentences(sents, true, 3)
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(sents.mkString("   "))) // spacing: sent=3
  }

  // mkDocumentFromTokens
  it should "make document from tokens, keep text" in {
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val text = toks.map(t => t.mkString(" ")).mkString(" ") // spacing: tok=1, sent=1
    val doc = client.mkDocumentFromTokens(toks, true)
    (doc) should not be (null)
    (doc.sentences.size) should equal(2)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "make document from tokens, discard text" in {
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val doc = client.mkDocumentFromTokens(toks, false)
    (doc) should not be (null)
    (doc.sentences.size) should equal(2)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "make document from tokens, keep text, add extra word spacing" in {
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
    val text = ""
    val reply = client.preprocessText(text)
    (reply) should not be (null)
    (reply) should equal(text)
  }

  it should "preprocess simple text" in {
    val text = "Testing is performed."
    val reply = client.preprocessText(text)
    (reply) should not be (null)
    (reply) should equal(text)
  }

  // preprocessSentences
  it should "preprocess sentences" in {
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
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val reply = client.preprocessTokens(toks)
    (reply.size) should equal(2)
    (reply.flatten) should equal(toks.flatten)
  }


  // tagPartsOfSpeech
  it should "tagPartsOfSpeech in a small document" in {
    val text = "This is a document with a single sentence."
    val doc1 = client.mkDocument(text, true)
    val doc = client.tagPartsOfSpeech(doc1)
    val sentences = doc.sentences
    (sentences.size) should equal(1)
    (sentences(0).tags) should not be (empty)
  }

  // lemmatize
  it should "lemmatize a small document" in {
    val text = "Children like smaller documents with smaller sentences."
    val doc1 = client.mkDocument(text, true)
    val doc2 = client.tagPartsOfSpeech(doc1)
    val doc = client.lemmatize(doc2)
    val sentences = doc.sentences
    (sentences.size) should equal(1)
    (sentences(0).lemmas) should not be (empty)
  }

  // recognizeNamedEntities
  it should "recognize named entities in a small document" in {
    val text = "On 6/8/2017, I sent a file containing some C# code to none@nowhere.com."
    val doc1 = client.mkDocument(text, true)
    val doc2 = client.tagPartsOfSpeech(doc1)
    val doc3 = client.lemmatize(doc2)
    val doc = client.recognizeNamedEntities(doc3)
    val sentences = doc.sentences
    (sentences.size) should equal(1)
    (sentences(0).tags) should not be (empty)
    (sentences(0).lemmas) should not be (empty)
    (sentences(0).entities) should not be (empty)
  }

  // parse
  it should "parse a small document" in {
    val text =
"""This document has multiple sentences. Each should be processed by the processor.
   A Reach document should be returned."""
    val doc1 = client.mkDocument(text, true)
    val doc = client.parse(doc1)
    val sentences = doc.sentences
    (sentences.size) should equal(3)
    (sentences(0).syntacticTree) should not be (empty)
    (sentences(1).syntacticTree) should not be (empty)
    (sentences(2).syntacticTree) should not be (empty)
  }

  // chunking
  it should "chunk a small document" in {
    val text = "Each document can contain many sentences. This document has two sentences."
    val doc1 = client.mkDocument(text, true)
    val doc2 = client.tagPartsOfSpeech(doc1)
    val doc = client.chunking(doc2)
    val sentences = doc.sentences
    (sentences.size) should equal(2)
    (sentences(0).tags) should not be (empty)
    (sentences(0).chunks) should not be (empty)
    (sentences(1).chunks) should not be (empty)
  }

  // resolveCoreference
  it should "resolve coreference in a small document" in {
    val text = "This is a document and it has one sentence and we like it."
    val doc1 = client.mkDocument(text, true)
    val doc2 = client.tagPartsOfSpeech(doc1)
    val doc3 = client.lemmatize(doc2)
    val doc4 = client.recognizeNamedEntities(doc3)
    val doc5 = client.parse(doc4)
    val doc = client.resolveCoreference(doc5)
    val sentences = doc.sentences
    (sentences.size) should equal(1)
    (doc.coreferenceChains) should be (empty) // BioNLP does not use coreference
  }

  // discourse
  it should "parse discourse in a small document" in {
    val text = "Despite what he said, this is a simple document containing small sentences."
    val doc1 = client.mkDocument(text, true)
    val doc2 = client.tagPartsOfSpeech(doc1)
    val doc3 = client.lemmatize(doc2)
    val doc4 = client.parse(doc3)
    val doc = client.discourse(doc4)
    // NOTE: following fails if the correct Processor type is not used:
    // (doc.discourseTree) should not be (empty)
  }


  // annotate(text)
  it should "annotate text, default keep" in {
    val text = "This is a document with a single sentence."
    val doc = client.annotate(text)
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "annotate text, keep text" in {
    val text = "This is single sentence test."
    val doc = client.annotate(text, true)   // explicit keep
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "annotate text, discard text" in {
    val text = "This is a document with a single sentence."
    val doc = client.annotate(text, false)  // explicit discard
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  // annotateFromSentences
  it should "annotate sentences, default keep" in {
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val doc = client.annotateFromSentences(sents)
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "annotate sentences, keep text" in {
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val doc = client.annotateFromSentences(sents, true) // explicit keep
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(sents.mkString(" ")))
  }

  it should "annotate sentences, discard text" in {
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    val doc = client.annotateFromSentences(sents, false) // explicit discard
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  // annotateFromTokens
  it should "annotate tokens, default keep" in {
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val doc = client.annotateFromTokens(toks)
    (doc) should not be (null)
    (doc.sentences.size) should equal(2)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "annotate tokens, keep text" in {
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val text = toks.map(t => t.mkString(" ")).mkString(" ")  // spacing: tok=1, sent=1
    val doc = client.annotateFromTokens(toks, true)       // explicit keep
    (doc) should not be (null)
    (doc.sentences.size) should equal(2)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "annotate tokens, discard text" in {
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val doc = client.annotateFromTokens(toks, false) // explicit discard
    (doc) should not be (null)
    (doc.sentences.size) should equal(2)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  // annotate(Document)
  it should "annotate zero-length Document, default keep" in {
    val docIn = client.mkDocument("")
    val doc = client.annotate(docIn)
    (doc) should not be (null)
    (doc.sentences.size) should equal(0)
    (doc.text).isDefined should be (false)
  }

  it should "annotate single sentence Document, no text to propagate" in {
    val text = "This is a document with a single sentence."
    val docIn = client.mkDocument(text)     // doc w/o kept text
    val doc = client.annotate(docIn)
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "annotate single sentence Document, keep text" in {
    val text = "This is a document with a single sentence."
    val docIn = client.mkDocument(text, true) // doc with text kept
    val doc = client.annotate(docIn)
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "annotate single sentence Document, discard text" in {
    val text = "This is a document with a single sentence."
    val docIn = client.mkDocument(text, false) // doc w/o kept text
    val doc = client.annotate(docIn)
    (doc) should not be (null)
    (doc.sentences.size) should equal(1)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "annotate multi-sentence Document, no text to propagate" in {
    val text = "This document has multiple sentences. Each should be processed by the processor. A Reach document should be returned."
    val docIn = client.mkDocument(text)    // doc w/o kept text
    val doc = client.annotate(docIn)
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

  it should "annotate multi-sentence Document, keep text" in {
    val text = "This document has multiple sentences. Each should be processed by the processor. A Reach document should be returned."
    val docIn = client.mkDocument(text, true) // doc with text kept
    val doc = client.annotate(docIn)
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (true)
    (doc.text) should equal(Some(text))
  }

  it should "annotate multi-sentence Document, discard text" in {
    val text = "This document has multiple sentences. Each should be processed by the processor. A Reach document should be returned."
    val docIn = client.mkDocument(text)    // doc w/o kept text
    val doc = client.annotate(docIn)
    (doc) should not be (null)
    (doc.sentences.size) should equal(3)
    (doc.text).isDefined should be (false)
    (doc.text) should equal(None)
  }

}
