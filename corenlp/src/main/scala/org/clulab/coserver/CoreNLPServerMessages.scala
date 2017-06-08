package org.clulab.coserver

import akka.actor.{ ActorRef, ActorSystem, Props, Actor }

import org.clulab.processors._

/**
  * Implement Akka message objects for the CoreNLP Server.
  *   Written by: Tom Hicks. 6/5/2017.
  *   Last Modified: Consolidate messages into one object.
  */
object CoreServerMessages {

  sealed trait CoreServerMessage

  // message for request side of server communication:
  sealed trait CoreProcessorCommand
  case class AnnotateCmd (text:String, keepText:Boolean = false) extends CoreProcessorCommand


  // messages for response side of server communication:
  sealed trait CoreProcessorReply
  case class TextMsg (text: String) extends CoreProcessorReply


  // messages which are both commands and replies
  case class DocumentMsg (doc: Document) extends CoreProcessorCommand with CoreProcessorReply

}
