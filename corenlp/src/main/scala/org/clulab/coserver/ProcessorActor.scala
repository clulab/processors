package org.clulab.coserver

import akka.actor.{ ActorRef, ActorSystem, Props, Actor }
import akka.event.Logging

import org.clulab.processors._
import org.clulab.processors.corenlp._

import CoreServerMessages._

/**
  * Actor which handles message to a Processor in the CoreNLPServer.
  *   Written by: Tom Hicks. 6/6/2017.
  *   Last Modified: Add tagPartsOfSpeech as model of side-effecting annotator methods.
  */
class ProcessorActor (

  /** The Processor to use within this actor. */
  val processor: Processor

) extends Actor {

  val log = Logging(context.system, this)

  def receive = {
    case cmd: AnnotateCmd => {
      log.info(s"Receive: annotate(doc=${cmd.doc}") // to DEBUG LATER
      val doc = processor.annotate(cmd.doc)
      sender ! DocumentMsg(doc)
    }

    case cmd: TagPartsOfSpeechCmd => {
      log.info(s"Receive: tagPartsOfSpeech(doc=${cmd.doc}") // to DEBUG LATER
      processor.tagPartsOfSpeech(cmd.doc)   // works by side-effect
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
   * Create Props for an actor of this type.
   *
   * @param processor The Processor to be passed to this actor’s constructor.
   * @return a Props for creating this actor.
   */
  def props (processor: Processor): Props = Props(new ProcessorActor(processor))

}
