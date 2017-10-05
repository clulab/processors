package org.clulab.processors.coclient

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
import org.clulab.processors.coshare.ProcessorCoreMessages._

/**
  * Client to access the Processors Core Server remotely using Akka.
  *   Written by: Tom Hicks. 6/9/2017.
  *   Last Modified: Update by extension of the Processor2 trait.
  */
object ProcessorCoreClient extends LazyLogging {

  // THE instance of the the processor core client
  private var _pcc: ProcessorCoreClient = _

  /** Create a single instance of the processor core client, only if it has not been created. */
  def instance: ProcessorCoreClient = {
    logger.debug(s"(ProcessorCoreClient.instance): pcc = ${_pcc}")
    if (_pcc == null) {                     // create client, iff not already created
      val config = ConfigFactory.load().getConfig("ProcessorCoreClient")
      if (config == null)
        throw new RuntimeException("(ProcessorCoreClient): Unable to read configuration from configuration file.")
      _pcc = new ProcessorCoreClient(config)
    }
    logger.debug(s"(ProcessorCoreClient.instance): pcc => ${_pcc}")
    _pcc
  }

  /** Expose the shutdown method from the instance. */
  def shutdown: Unit = instance.shutdown
}


class ProcessorCoreClient (

  /** Application-specific portion of the configuration file. */
  val config: Config

) extends Processor2 with LazyLogging {

  // read actor system name from the configuration file
  private val systemName = if (config.hasPath("server.systemName"))
                             config.getString("server.systemName")
                           else "procCoreServer"

  // fire up the actor system
  private val system = ActorSystem(systemName)
  logger.debug(s"system=${system}")

  // fire up the processor core server and get a ref to the message router
  val router: ActorRef = getRouterRef(config)

  /** Acquire actor ref via actor selection on the configured server path. */
  private def getRouterRef (config: Config): ActorRef = {
    val serverPath = if (config.hasPath("server.path")) config.getString("server.path")
                     else s"akka://${systemName}/user/procActorPool"
    val ref = system.actorSelection(ActorPath.fromString(serverPath)).resolveOne()
    Await.result(ref, 1.minute).asInstanceOf[ActorRef]
  }

  // simulate blocking RPC: finite duration is required so make it long
  implicit val timeout = Timeout(8 hours)  // time limit to return Future from call

  /** Send the given message to the server and block until response comes back. */
  private def callServer (request: ProcessorCoreCommand): ProcessorCoreReply = {
    val response = router ? request         // call returns Future within long timeout
    val result = Await.result(response, Duration.Inf) // blocking: wait forever
    if (result.isInstanceOf[ServerExceptionMsg]) {
      val exception = result.asInstanceOf[ServerExceptionMsg].exception
      throw new RuntimeException(exception.getMessage())
    }
    else
      result.asInstanceOf[ProcessorCoreReply]
  }

  /** Send the core server a message to shutdown actors and terminate the actor system. */
  def shutdown: Unit = {
    if (config.getBoolean("shutdownServerOnExit")) {
      router ! Broadcast(PoisonPill)
      router ! PoisonPill
    }
  }


  /** Constructs a document of tokens from free text; includes sentence splitting and tokenization */
  def mkDocument (text:String, keepText:Boolean = false): Document = {
    val reply = callServer(MkDocumentCmd(text, keepText))
    reply.asInstanceOf[DocumentMsg].doc
  }

  /** Constructs a document of tokens from an array of untokenized sentences */
  def mkDocumentFromSentences (
    sentences: Iterable[String],
    keepText: Boolean = false,
    charactersBetweenSentences: Int = 1
  ): Document = {
    val reply = callServer(
      MkDocumentFromSentencesCmd(sentences, keepText, charactersBetweenSentences))
    reply.asInstanceOf[DocumentMsg].doc
  }

  /** Constructs a document of tokens from an array of tokenized sentences */
  def mkDocumentFromTokens (
    sentences: Iterable[Iterable[String]],
    keepText: Boolean = false,
    charactersBetweenSentences: Int = 1,
    charactersBetweenTokens: Int = 1
  ): Document = {
    val reply = callServer(
      MkDocumentFromTokensCmd(sentences, keepText,
                              charactersBetweenSentences, charactersBetweenTokens))
    reply.asInstanceOf[DocumentMsg].doc
  }

  /**
    * Hook to allow the preprocessing of input text
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


  /** Part of speech tagging. Modified document is returned. */
  def tagPartsOfSpeech (doc:Document): Document = {
    val reply = callServer(TagPartsOfSpeechCmd(doc))
    reply.asInstanceOf[DocumentMsg].doc
  }

  /** Lematization. Modified document is returned. */
  def lemmatize (doc:Document): Document = {
    val reply = callServer(LemmatizeCmd(doc))
    reply.asInstanceOf[DocumentMsg].doc
  }

  /** NER - Named Entity Recognition. Modified document is returned. */
  def recognizeNamedEntities (doc:Document): Document = {
    val reply = callServer(RecognizeNamedEntitiesCmd(doc))
    reply.asInstanceOf[DocumentMsg].doc
  }

  /** Syntactic parsing. Modified document is returned. */
  def parse (doc:Document): Document = {
    val reply = callServer(ParseCmd(doc))
    reply.asInstanceOf[DocumentMsg].doc
  }

  /** Shallow parsing. Modified document is returned. */
  def chunking (doc:Document): Document = {
    val reply = callServer(ChunkingCmd(doc))
    reply.asInstanceOf[DocumentMsg].doc
  }

  /** Coreference resolution. Modified document is returned. */
  def resolveCoreference (doc:Document): Document = {
    val reply = callServer(ResolveCoreferenceCmd(doc))
    reply.asInstanceOf[DocumentMsg].doc
  }

  /** Discourse parsing. Modified document is returned. */
  def discourse (doc:Document): Document = {
    val reply = callServer(DiscourseCmd(doc))
    reply.asInstanceOf[DocumentMsg].doc
  }


  override def annotate (text:String, keepText:Boolean = false): Document = {
    val reply = callServer(AnnotateTextCmd(text, keepText))
    reply.asInstanceOf[DocumentMsg].doc
  }

  override def annotateFromSentences (sentences:Iterable[String], keepText:Boolean = false): Document = {
    val reply = callServer(AnnotateFromSentencesCmd(sentences, keepText))
    reply.asInstanceOf[DocumentMsg].doc
  }

  override def annotateFromTokens (
    sentences:Iterable[Iterable[String]],
    keepText:Boolean = false
  ): Document = {
    val reply = callServer(AnnotateFromTokensCmd(sentences, keepText))
    reply.asInstanceOf[DocumentMsg].doc
  }

  override def annotate (doc:Document): Document = {
    val reply = callServer(AnnotateCmd(doc))
    reply.asInstanceOf[DocumentMsg].doc
  }

  // Only for error testing -- should not be exposed as part of API
  // def errorTest: Unit = {
  //   callServer(ErrorTestCmd())              // should throw RuntimeException
  // }

}
