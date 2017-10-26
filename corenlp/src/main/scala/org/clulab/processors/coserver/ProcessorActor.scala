package org.clulab.processors.coserver

import akka.actor.{ ActorRef, Props, Actor }
import akka.event.Logging

import org.clulab.processors._
import org.clulab.processors.corenlp._
import org.clulab.processors.coshare.ProcessorCoreMessages._
import org.clulab.utils.StringUtils

/**
  * Actor which handles message to a Processor in the CoreNLPServer.
  *   Written by: Tom Hicks. 6/6/2017.
  *   Last Modified: Expand processing chain for side-effecting calls.
  */
class ProcessorActor (

  /** The Processor to use within this actor. */
  val processor: Processor

) extends Actor {

  val log = Logging(context.system, this)

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
    case cmd: MkDocumentCmd =>
      log.debug(s"(ProcessorActor.receive): mkDocument(text=${cmd.text}, keep=${cmd.keepText})")
      try {
        val doc = processor.mkDocument(cmd.text, cmd.keepText)
        sender ! DocumentMsg(Document(doc))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.MkDocumentCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: MkDocumentFromSentencesCmd =>
      log.debug(s"(ProcessorActor.receive): mkDocumentFromSentences(sents=${cmd.sentences}, keep=${cmd.keepText}, charsBTWSents=${cmd.charactersBetweenSentences})")
      try {
        val doc = processor.mkDocumentFromSentences(
          cmd.sentences, cmd.keepText, cmd.charactersBetweenSentences)
        sender ! DocumentMsg(Document(doc))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.MkDocumentFromSentencesCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: MkDocumentFromTokensCmd =>
      log.debug(s"(ProcessorActor.receive): mkDocumentFromTokens(sents=${cmd.sentences}, keep=${cmd.keepText}, charsBTWSents=${cmd.charactersBetweenSentences}, charsBTWToks=${cmd.charactersBetweenTokens})")
      try {
        val doc = processor.mkDocumentFromTokens(
          cmd.sentences, cmd.keepText, cmd.charactersBetweenSentences, cmd.charactersBetweenTokens)
        sender ! DocumentMsg(Document(doc))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.MkDocumentFromTokensCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: PreprocessTextCmd =>
      log.debug(s"(ProcessorActor.receive): preprocessText(text=${cmd.text})")
      try {
        val pptext = processor.preprocessText(cmd.text)
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
        val ppsents = processor.preprocessSentences(cmd.sentences)
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
        val pptoks = processor.preprocessTokens(cmd.sentences)
        sender ! TokensMsg(pptoks)
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.PreprocessTokensCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: TagPartsOfSpeechCmd =>
      log.debug(s"(ProcessorActor.receive): tagPartsOfSpeech(doc=${cmd.doc})")
      try {
        val doc = processor.mkDocument(cmd.doc.text.getOrElse(""))
        processor.tagPartsOfSpeech(doc)     // works by side-effect
        sender ! DocumentMsg(Document(doc))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.TagPartsOfSpeechCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: LemmatizeCmd =>
      log.debug(s"(ProcessorActor.receive): lemmatize(doc=${cmd.doc})")
      try {
        val doc = processor.mkDocument(cmd.doc.text.getOrElse(""))
        processor.tagPartsOfSpeech(doc)     // works by side-effect
        processor.lemmatize(doc)            // works by side-effect
        sender ! DocumentMsg(Document(doc))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.LemmatizeCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: RecognizeNamedEntitiesCmd =>
      log.debug(s"(ProcessorActor.receive): recognizeNamedEntities(doc=${cmd.doc})")
      try {
        val doc = processor.mkDocument(cmd.doc.text.getOrElse(""))
        processor.tagPartsOfSpeech(doc)       // works by side-effect
        processor.lemmatize(doc)              // works by side-effect
        processor.recognizeNamedEntities(doc) // works by side-effect
        sender ! DocumentMsg(Document(doc))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.RecognizeNamedEntitiesCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: ParseCmd =>
      log.debug(s"(ProcessorActor.receive): parse(doc=${cmd.doc})")
      try {
        val doc = processor.mkDocument(cmd.doc.text.getOrElse(""))
        processor.tagPartsOfSpeech(doc)       // works by side-effect
        processor.lemmatize(doc)              // works by side-effect
//        processor.recognizeNamedEntities(doc) // works by side-effect
        processor.parse(doc)                  // works by side-effect
        sender ! DocumentMsg(Document(doc))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.ParseCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: ChunkingCmd =>
      log.debug(s"(ProcessorActor.receive): chunking(doc=${cmd.doc})")
      try {
        val doc = processor.mkDocument(cmd.doc.text.getOrElse(""))
        processor.tagPartsOfSpeech(doc)       // works by side-effect
        processor.chunking(doc)               // works by side-effect
        sender ! DocumentMsg(Document(doc))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.ChunkingCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: ResolveCoreferenceCmd =>
      log.debug(s"(ProcessorActor.receive): resolveCoreference(doc=${cmd.doc})")
      try {
        val doc = processor.mkDocument(cmd.doc.text.getOrElse(""))
        processor.tagPartsOfSpeech(doc)       // works by side-effect
        processor.lemmatize(doc)              // works by side-effect
        processor.recognizeNamedEntities(doc) // works by side-effect
        processor.parse(doc)                  // works by side-effect
        processor.resolveCoreference(doc)     // works by side-effect
        sender ! DocumentMsg(Document(doc))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.ResolveCoreferenceCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: DiscourseCmd =>
      log.debug(s"(ProcessorActor.receive): discourse(doc=${cmd.doc})")
      try {
        val doc = processor.mkDocument(cmd.doc.text.getOrElse(""))
        processor.tagPartsOfSpeech(doc)       // works by side-effect
        processor.lemmatize(doc)              // works by side-effect
        processor.recognizeNamedEntities(doc) // works by side-effect
        processor.parse(doc)                  // works by side-effect
        processor.discourse(doc)              // works by side-effect
        sender ! DocumentMsg(Document(doc))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.DiscourseCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }


    case cmd: AnnotateFromSentencesCmd =>
      log.debug(s"(ProcessorActor.receive): annotateFromSentences(sents=${cmd.sentences}, keep=${cmd.keepText})")
      try {
        val doc = processor.annotateFromSentences(cmd.sentences, cmd.keepText)
        sender ! DocumentMsg(Document(doc))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.AnnotateFromSentencesCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: AnnotateFromTokensCmd =>
      log.debug(s"(ProcessorActor.receive): annotateFromTokens(sents=${cmd.sentences}, keep=${cmd.keepText})")
      try {
        val doc = processor.annotateFromTokens(cmd.sentences, cmd.keepText)
        sender ! DocumentMsg(Document(doc))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.AnnotateFromTokensCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: AnnotateTextCmd =>
      log.debug(s"(ProcessorActor.receive): annotateText(text=${cmd.text}, keep=${cmd.keepText})")
      try {
        val doc = processor.annotate(cmd.text, cmd.keepText)
        sender ! DocumentMsg(Document(doc))
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.AnnotateTextCmd): ${StringUtils.exceptionToString(ex)}")
          sender ! ServerExceptionMsg(ex)
        }
      }

    case cmd: AnnotateCmd =>
      log.debug(s"(ProcessorActor.receive): annotate(doc=${cmd.doc})")
      try {
        val doc = if (cmd.doc.text.isDefined)
          processor.annotate(cmd.doc.text.get, true)
        // else if (!cmd.doc.sentences.isEmpty)      // FIX LATER
        //   processor.annotateFromSentences(cmd.doc.sentences.toIterable, true)
        else
          processor.annotate("", true)
        val retDoc = Document(doc)
        log.error(s"(AnnotateCmd):   RETDOC=${retDoc.sentences.size}") // REMOVE LATER
        log.error(s"(AnnotateCmd):   RETDOC=${retDoc.text}")           // REMOVE LATER
        sender ! DocumentMsg(retDoc)
      } catch {
        case ex:Exception => {
          log.error(s"(ProcessorActor.AnnotateCmd): ${StringUtils.exceptionToString(ex)}")
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
   *   @param processor The Processor to be passed to this actor’s constructor.
   *   @return a Props for creating this actor.
   */
  def props (processor: Processor): Props = Props(new ProcessorActor(processor))
}
