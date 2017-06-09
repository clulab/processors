package org.clulab.coserver

import akka.actor.{ ActorRef, ActorSystem, Props, Actor }
import akka.event.Logging

import org.clulab.processors._
import org.clulab.processors.corenlp._

import ProcessorCoreServerMessages._

/**
  * Actor which handles message to a Processor in the CoreNLPServer.
  *   Written by: Tom Hicks. 6/6/2017.
  *   Last Modified: Cleanup log & doc strings.
  */
class ProcessorActor (

  /** The Processor to use within this actor. */
  val processor: Processor

) extends Actor {

  val log = Logging(context.system, this)

  def receive = {
    case cmd: AnnotateCmd => {
      log.info(s"(ProcessorActor.receive): annotate(doc=${cmd.doc}") // set to DEBUG LATER
      val doc = processor.annotate(cmd.doc)
      sender ! DocumentMsg(doc)
    }

    case cmd: TagPartsOfSpeechCmd => {
      log.info(s"(ProcessorActor.receive): tagPartsOfSpeech(doc=${cmd.doc}") // set to DEBUG LATER
      processor.tagPartsOfSpeech(cmd.doc)   // works by side-effect
      sender ! DocumentMsg(cmd.doc)
    }

    case cmd: LemmatizeCmd => {
      log.info(s"(ProcessorActor.receive): lemmatize(doc=${cmd.doc}") // set to DEBUG LATER
      processor.lemmatize(cmd.doc)          // works by side-effect
      sender ! DocumentMsg(cmd.doc)
    }

    case cmd: RecognizeNamedEntitiesCmd => {
      log.info(s"(ProcessorActor.receive): recognizeNamedEntities(doc=${cmd.doc}") // set to DEBUG LATER
      processor.recognizeNamedEntities(cmd.doc)  // works by side-effect
      sender ! DocumentMsg(cmd.doc)
    }

    case cmd: ParseCmd => {
      log.info(s"(ProcessorActor.receive): parse(doc=${cmd.doc}") // set to DEBUG LATER
      processor.parse(cmd.doc)              // works by side-effect
      sender ! DocumentMsg(cmd.doc)
    }

    case cmd: ChunkingCmd => {
      log.info(s"(ProcessorActor.receive): chunking(doc=${cmd.doc}") // set to DEBUG LATER
      processor.chunking(cmd.doc)           // works by side-effect
      sender ! DocumentMsg(cmd.doc)
    }

    case cmd: LabelSemanticRolesCmd => {
      log.info(s"(ProcessorActor.receive): labelSemanticRoles(doc=${cmd.doc}") // set to DEBUG LATER
      processor.labelSemanticRoles(cmd.doc) // works by side-effect
      sender ! DocumentMsg(cmd.doc)
    }

    case cmd: ResolveCoreferenceCmd => {
      log.info(s"(ProcessorActor.receive): resolveCoreference(doc=${cmd.doc}") // set to DEBUG LATER
      processor.resolveCoreference(cmd.doc) // works by side-effect
      sender ! DocumentMsg(cmd.doc)
    }

    case cmd: DiscourseCmd => {
      log.info(s"(ProcessorActor.receive): discourse(doc=${cmd.doc}") // set to DEBUG LATER
      processor.discourse(cmd.doc)          // works by side-effect
      sender ! DocumentMsg(cmd.doc)
    }

    case unknown => {
      log.error(s"ProcessorActor: unrecognized message: ${unknown}")
      sender ! TextMsg(s"ProcessorActor: unrecognized message: ${unknown}")
    }
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
