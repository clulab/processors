package org.clulab.coserver

import akka.actor.{ ActorRef, ActorSystem, Props, Actor }
import akka.event.Logging

import org.clulab.processors._
import org.clulab.processors.corenlp._

import CoreProcessorCommands._
import CoreProcessorReplies._

/**
  * Actor which handles message to a Processor in the CoreNLPServer.
  *   Written by: Tom Hicks. 6/6/2017.
  *   Last Modified: Initial creation.
  */
class ProcessorActor (

  /** The Processor to use within this actor. */
  val processor: Processor

) extends Actor {

  val log = Logging(context.system, this)

  def receive = {
    case cmd: AnnotateCmd =>
      log.info(s"Receive: annotate(text=${cmd.text}, keep=${cmd.keepText}")
    case unknown =>
      log.warning(s"Receive: unrecognized msg: ${unknown}")
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
