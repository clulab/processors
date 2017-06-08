package org.clulab.coserver

import akka.actor.{ ActorRef, ActorSystem, Props, Actor }
import akka.event.Logging

import org.clulab.processors._
import org.clulab.processors.corenlp._

import CoreServerMessages._

/**
  * Actor which handles message to a Processor in the CoreNLPServer.
  *   Written by: Tom Hicks. 6/6/2017.
  *   Last Modified: Update for message consolidation.
  */
class ProcessorActor (

  /** The Processor to use within this actor. */
  val processor: Processor

) extends Actor {

  val log = Logging(context.system, this)

  def receive = {
    case cmd: AnnotateCmd => {
      log.info(s"Receive: annotate(text=${cmd.text}, keep=${cmd.keepText}")
      val doc = processor.annotate(cmd.text, cmd.keepText)
      sender ! DocumentMsg(doc)
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
