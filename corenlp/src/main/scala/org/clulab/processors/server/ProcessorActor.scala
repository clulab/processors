package org.clulab.processors.server

import akka.actor.{ ActorRef, Props, Actor }
import akka.event.Logging

import org.clulab.processors._
import org.clulab.processors.corenlp._
import org.clulab.processors.csshare.ProcessorCSMessages._
import org.clulab.serialization.DocumentSerializer
import org.clulab.utils.StringUtils

/**
  * Actor which handles message to a Processor in the Processors Client/Server.
  *   Written by: Tom Hicks. 6/6/2017.
  *   Last Modified: Update for UTF-8 default and keepText flag in serializer.
  */
class ProcessorActor (

  /** The annotation subset of Processor to use within this actor. */
  val procAnnotator: ProcessorAnnotator

) extends Actor {

  val log = Logging(context.system, this)

  // serializer for serializing response Documents
  private val serializer = new DocumentSerializer

  // lifecycle methods for tracing
  override def preStart (): Unit =
    log.debug(s"Actor ${self} starting")

  override def preRestart (reason: Throwable, msg: Option[Any]): Unit = {
    log.debug(s"Actor ${self} restarting...")
    super.preRestart(reason, msg)
  }

  override def postRestart (reason: Throwable): Unit = {
    log.debug(s"Actor ${self} ...restarted")
    super.postRestart(reason)
  }

  override def postStop (): Unit =
    log.debug(s"Actor ${self} stopped")


  def receive = {

    case cmd: AnnotateFromSentencesCmd =>
      log.debug(s"(ProcessorActor.receive): annotateFromSentences(sents=${cmd.sentences}, keep=${cmd.keepText})")
      try {
        val doc = procAnnotator.annotateFromSentences(cmd.sentences, cmd.keepText)
        sender ! TextMsg(serializer.save(doc, keepText=cmd.keepText))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.AnnotateFromSentencesCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: AnnotateFromTokensCmd =>
      log.debug(s"(ProcessorActor.receive): annotateFromTokens(sents=${cmd.sentences}, keep=${cmd.keepText})")
      try {
        val doc = procAnnotator.annotateFromTokens(cmd.sentences, cmd.keepText)
        sender ! TextMsg(serializer.save(doc, keepText=cmd.keepText))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.AnnotateFromTokensCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: AnnotateTextCmd =>
      log.debug(s"(ProcessorActor.receive): annotateText(text=${cmd.text}, keep=${cmd.keepText})")
      try {
        val doc = procAnnotator.annotate(cmd.text, cmd.keepText)
        sender ! TextMsg(serializer.save(doc, keepText=cmd.keepText))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.AnnotateTextCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    // case cmd: AnnotateCmd =>
    //   log.debug(s"(ProcessorActor.receive): annotate(text=${cmd.text}, keep=${cmd.keepText})")
    //   try {
    //     val doc = procAnnotator.annotate(cmd.text, cmd.keepText)
    //     sender ! TextMsg(serializer.save(doc, keepText=cmd.keepText))
    //   } catch {
    //     case ex:Exception => {
    //       log.error(s"(ProcessorActor.AnnotateCmd): ${StringUtils.exceptionToString(ex)}")
    //       sender ! ServerExceptionMsg(ex)
    //     }
    //   }

    case cmd: PreprocessTextCmd =>
      log.debug(s"(ProcessorActor.receive): preprocessText(text=${cmd.text})")
      try {
        val pptext = procAnnotator.preprocessText(cmd.text)
        sender ! TextMsg(pptext)
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.PreprocessTextCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: PreprocessSentencesCmd =>
      log.debug(s"(ProcessorActor.receive): preprocessSentences(sents=${cmd.sentences})")
      try {
        val ppsents = procAnnotator.preprocessSentences(cmd.sentences)
        sender ! SentencesMsg(ppsents)
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.PreprocessSentencesCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: PreprocessTokensCmd =>
      log.debug(s"(ProcessorActor.receive): preprocessTokens(sents=${cmd.sentences})")
      try {
        val pptoks = procAnnotator.preprocessTokens(cmd.sentences)
        sender ! TokensMsg(pptoks)
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.PreprocessTokensCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: ErrorTestCmd =>
      log.error(s"(ProcessorActor.receive): ErrorTest command")
      try {
        throw new RuntimeException("This is a fake error from the ErrorTest command.")
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.ErrorTestCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case unknown =>
      log.error(s"ProcessorActor: received unrecognized message: ${unknown}")
      sender ! ServerExceptionMsg(
        new RuntimeException(s"ProcessorActor: received unrecognized message: ${unknown}"))
  }
}

object ProcessorActor {
  /**
   * Constructor to create Props for an actor of this type.
   *   @param procAnnotator The ProcessorAnnotator to be passed to this actor’s constructor.
   *   @return a Props for creating this actor.
   */
  def props (procAnnotator: ProcessorAnnotator): Props = Props(new ProcessorActor(procAnnotator))
}
