package org.clulab.coserver

import akka.actor.{ ActorRef, ActorSystem, Props, Actor }

import org.clulab.processors._

/**
  * Implement Akka message objects for the CoreNLP Server.
  *   Written by: Tom Hicks. 6/5/2017.
  *   Last Modified: Add side-effecting annotator messages.
  */
object CoreServerMessages {

  sealed trait CoreServerMessage

  // message for request side of server communication:
  sealed trait CoreProcessorCommand
  case class AnnotateCmd (doc:Document) extends CoreProcessorCommand
  case class TagPartsOfSpeechCmd (doc:Document) extends CoreProcessorCommand
  case class LemmatizeCmd (doc:Document) extends CoreProcessorCommand
  case class RecognizeNamedEntitiesCmd (doc:Document) extends CoreProcessorCommand
  case class ParseCmd (doc:Document) extends CoreProcessorCommand
  case class ChunkingCmd (doc:Document) extends CoreProcessorCommand
  case class LabelSemanticRolesCmd (doc:Document) extends CoreProcessorCommand
  case class ResolveCoreferenceCmd (doc:Document) extends CoreProcessorCommand
  case class DiscourseCmd (doc:Document) extends CoreProcessorCommand


  // messages for response side of server communication:
  sealed trait CoreProcessorReply
  case class TextMsg (text: String) extends CoreProcessorReply


  // messages which are both commands and replies
  case class DocumentMsg (doc: Document) extends CoreProcessorCommand with CoreProcessorReply

}
