package org.clulab.processors.coserver

import scala.concurrent.duration._

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.actor.{ ActorSystem, Props, Actor }
import akka.testkit.{ TestKit, TestActorRef, TestProbe, ImplicitSender }
import org.scalatest.{ BeforeAndAfterAll, FlatSpecLike, MustMatchers }

import org.clulab.processors._
import org.clulab.processors.bionlp._
import org.clulab.processors.corenlp._
import org.clulab.processors.fastnlp._
import org.clulab.processors.shallownlp._
import org.clulab.processors.coshare.ProcessorCoreMessages._

/**
  * Unit tests of the ProcessorActor class.
  *   Written by: Tom Hicks. 6/6/2017.
  *   Last Modified: Update for implementation of processor annotator trait only.
  */
class TestProcessorActor extends TestKit(ActorSystem("test-proc-actor"))
    with FlatSpecLike
    with ImplicitSender
    with BeforeAndAfterAll
    with MustMatchers
    with LazyLogging
{
  val config = ConfigFactory.load().getConfig("ProcessorCoreServer")

  // shutdown the actor system when done testing
  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  // read which processor type is specified by the configuration
  val procType = config.getString("server.processor.type")
  logger.debug(s"processor type=${procType}")

  // set default timeout: raise it for some processors (below)
  var timeout: FiniteDuration = 30.seconds

  // set default use of co-reference to false: only CoreNLP seems to use it
  var usesCoreference = false

  // create the Processor engine used by this server. increase timeout for slow processors.
  val processor: Processor = {
    procType.toLowerCase match {
      case "bio" =>
        timeout = 3.minutes
        new BioNLPProcessor(removeFigTabReferences = true)

      case "core" =>
        usesCoreference = true
        new CoreNLPProcessor()

      case "fast" =>
        new FastNLPProcessor()

      case "fastbio" =>
        timeout = 3.minutes
        new FastBioNLPProcessor(removeFigTabReferences = true)

      case _ => new ShallowNLPProcessor()
    }
  }

  val procActor = system.actorOf(ProcessorActor.props(processor))
  logger.debug(s"actor=${procActor}")

  // ErrorTest
  "ProcessorActor" should "return error upon error test" in {
    val probe = TestProbe()
    probe.send(procActor, ErrorTestCmd())
    val reply = probe.expectMsgClass(timeout, classOf[ServerExceptionMsg])
    (reply) must not be (null)
    (reply.exception) must not be (null)
    (reply.exception.isInstanceOf[RuntimeException]) must be (true)
    (reply.exception.getMessage()) must be ("This is a fake error from the ErrorTest command.")
  }

  // error on unknown message
  it should "return error upon unknown message" in {
    val probe = TestProbe()
    probe.send(procActor, "UNKNOWN")
    val reply = probe.expectMsgClass(timeout, classOf[ServerExceptionMsg])
    (reply) must not be (null)
    (reply.exception) must not be (null)
    (reply.exception.isInstanceOf[RuntimeException]) must be (true)
    (reply.exception.getMessage()) must be ("ProcessorActor: received unrecognized message: UNKNOWN")
  }

  // annotate(text)
  it should "annotate text, default keep" in {
    val probe = TestProbe()
    val text = "This is a document with a single sentence."
    probe.send(procActor, AnnotateTextCmd(text))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(1)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

  it should "annotate text, keep text" in {
    val probe = TestProbe()
    val text = "This is single sentence test."
    probe.send(procActor, AnnotateTextCmd(text, true)) // explicit keep
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(1)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(text))
  }

  it should "annotate text, discard text" in {
    val probe = TestProbe()
    val text = "This is a document with a single sentence."
    probe.send(procActor, AnnotateTextCmd(text, false)) // explicit discard
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(1)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

  // annotateFromSentences
  it should "annotate sentences, default keep" in {
    val probe = TestProbe()
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    probe.send(procActor, AnnotateFromSentencesCmd(sents))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(3)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

  it should "annotate sentences, keep text" in {
    val probe = TestProbe()
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    probe.send(procActor, AnnotateFromSentencesCmd(sents, true)) // explicit keep
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(3)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(sents.mkString(" ")))
  }

  it should "annotate sentences, discard text" in {
    val probe = TestProbe()
    val sents = Seq("This is a test.", "It is only a test.", "In the event of a real document.")
    probe.send(procActor, AnnotateFromSentencesCmd(sents, false)) // explicit discard
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(3)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }


  // annotateFromTokens
  it should "annotate tokens, default keep" in {
    val probe = TestProbe()
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    probe.send(procActor, AnnotateFromTokensCmd(toks))
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(2)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

  it should "annotate tokens, keep text" in {
    val probe = TestProbe()
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    val text = toks.map(t => t.mkString(" ")).mkString(" ")  // spacing: tok=1, sent=1
    probe.send(procActor, AnnotateFromTokensCmd(toks, true)) // explicit keep
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(2)
    (reply.doc.text).isDefined must be (true)
    (reply.doc.text) must equal(Some(text))
  }

  it should "annotate tokens, discard text" in {
    val probe = TestProbe()
    val toks = Seq(Seq("This", "is", "a", "test."), Seq("It", "is", "only", "a", "test."))
    probe.send(procActor, AnnotateFromTokensCmd(toks, false)) // explicit discard
    val reply = probe.expectMsgClass(timeout, classOf[DocumentMsg])
    (reply.doc.sentences.size) must equal(2)
    (reply.doc.text).isDefined must be (false)
    (reply.doc.text) must equal(None)
  }

}
