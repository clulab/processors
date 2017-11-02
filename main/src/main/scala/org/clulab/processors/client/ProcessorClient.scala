package org.clulab.processors.client

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

import com.typesafe.config.{ Config, ConfigValueFactory, ConfigFactory }
import com.typesafe.scalalogging.LazyLogging

import akka.actor._
import akka.pattern.ask
import akka.routing.Broadcast
import akka.util.Timeout

import org.clulab.processors._
import org.clulab.processors.csshare.ProcessorCSMessages._

/**
  * Client to access the Processors Server remotely using Akka.
  *   Written by: Tom Hicks. 6/9/2017.
  *   Last Modified: Separate client and server shutdown methods.
  */
object ProcessorClient extends LazyLogging {

  // THE instance of the the processor client
  private var _pcc: ProcessorClient = _

  /** Create a single instance of the processor client, only if it has not been created. */
  def instance: ProcessorClient = {
    logger.debug(s"(ProcessorClient.instance): pcc = ${_pcc}")
    if (_pcc == null) {                     // create client, iff not already created
      val config = ConfigFactory.load().getConfig("ProcessorClient")
      if (config == null)
        throw new RuntimeException("(ProcessorClient): Unable to read configuration from configuration file.")
      _pcc = new ProcessorClient(config)
    }
    logger.debug(s"(ProcessorClient.instance): pcc => ${_pcc}")
    _pcc
  }

  /** Expose the shutdown method from the instance. */
  def shutdown: Unit = instance.shutdown
}


class ProcessorClient (

  /** Application-specific portion of the configuration file. */
  val config: Config

) extends ProcessorAnnotator with LazyLogging {

  private val connectTime = 30.seconds

  logger.debug(s"(ProcessorClient): config=${config}")

  // fire up the actor system
  val system = ActorSystem("procClient", config)
  logger.debug(s"(ProcessorClient): system=${system}")

  // simulate blocking RPC: finite duration is required so make it long
  implicit val timeout = Timeout(8 hours)  // time limit to return Future from call

  // fire up the processor server and get a ref to the message router
  val router: ActorRef = getRouterRef(config)

  /** Acquire actor ref via actor selection on the configured server path. */
  private def getRouterRef (config: Config): ActorRef = {
    val serverPath = if (config.hasPath("server.path"))
      config.getString("server.path")
    else
      throw new RuntimeException("(ProcessorClient): Configuration file must define server.path")
    val ref = system.actorSelection(ActorPath.fromString(serverPath)).resolveOne(connectTime)
    Await.result(ref, connectTime).asInstanceOf[ActorRef]
  }

  /** Send the given message to the server and block until response comes back. */
  private def callServer (request: ProcessorCSCommand): ProcessorCSReply = {
    val response = router ? request         // call returns Future within long timeout
    val result = Await.result(response, Duration.Inf) // blocking: wait forever
    if (result.isInstanceOf[ServerExceptionMsg]) {
      val exception = result.asInstanceOf[ServerExceptionMsg].exception
      throw new RuntimeException(exception)
    }
    else
      result.asInstanceOf[ProcessorCSReply]
  }

  /** Shutdown this clients actors and terminate the actor system. */
  def shutdown: Unit = {
    router ! Broadcast(PoisonPill)
    system.terminate()
  }

  /** Send the server a message to shutdown actors and terminate the actor system. */
  def shutdownServer: Unit = {
    if (config.getBoolean("shutdownServerOnExit")) {
      router ! Broadcast(PoisonPill)
      router ! PoisonPill
    }
  }


  /** Annotate the given text string, specify whether to retain the text in the resultant Document. */
  override def annotate (text:String, keepText:Boolean = false): Document = {
    val reply = callServer(AnnotateTextCmd(text, keepText))
    reply.asInstanceOf[DocumentMsg].doc
  }

  /** Annotate the given sentences, specify whether to retain the text in the resultant Document. */
  override def annotateFromSentences (
    sentences:Iterable[String],
    keepText:Boolean = false): Document =
  {
    val reply = callServer(AnnotateFromSentencesCmd(sentences, keepText))
    reply.asInstanceOf[DocumentMsg].doc
  }

  /** Annotate the given tokens, specify whether to retain the text in the resultant Document. */
  override def annotateFromTokens (
    sentences:Iterable[Iterable[String]],
    keepText:Boolean = false
  ): Document = {
    val reply = callServer(AnnotateFromTokensCmd(sentences, keepText))
    reply.asInstanceOf[DocumentMsg].doc
  }


  /**
    * Hook to allow the preprocessing of input text.
    * @param origText The original input text
    * @return The preprocessed text
    */
  override def preprocessText (origText:String): String = {
    val reply = callServer(PreprocessTextCmd(origText))
    reply.asInstanceOf[TextMsg].text
  }

  /** Runs preprocessText on each sentence */
  override def preprocessSentences (origSentences:Iterable[String]): Iterable[String] = {
    val reply = callServer(PreprocessSentencesCmd(origSentences))
    reply.asInstanceOf[SentencesMsg].sentences
  }

  /** Runs preprocessText on each token */
  override def preprocessTokens (origSentences:Iterable[Iterable[String]]): Iterable[Iterable[String]] = {
    val reply = callServer(PreprocessTokensCmd(origSentences))
    reply.asInstanceOf[TokensMsg].tokens
  }


  // Only for error testing -- should not be exposed as part of API
  // def errorTest: Unit = {
  //   callServer(ErrorTestCmd())              // should throw RuntimeException
  // }

}
