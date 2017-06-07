package org.clulab.coserver

// import com.typesafe.scalalogging.LazyLogging

import akka.actor.{ ActorRef, ActorSystem, Props, Actor }

import org.clulab.processors._

/**
  * Implement Akka message objects for the CoreNLP Server.
  *   Written by: Tom Hicks. 6/5/2017.
  *   Last Modified: Add text reply message.
  */
object CoreServerMessages {

  sealed trait CoreServerMessage

}


object CoreProcessorCommands {

  sealed trait CoreProcessorCommand
  case class AnnotateCmd (text:String, keepText:Boolean = false) extends CoreProcessorCommand

}


object CoreProcessorReplies {

  sealed trait CoreProcessorReply
  case class DocumentMsg (doc: Document) extends CoreProcessorReply
  case class TextMsg (text: String) extends CoreProcessorReply

}
