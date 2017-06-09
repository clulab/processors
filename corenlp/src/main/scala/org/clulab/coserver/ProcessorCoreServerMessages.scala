package org.clulab.coserver

import akka.actor.{ ActorRef, ActorSystem, Props, Actor }

import org.clulab.processors._

/**
  * Implement Akka message objects for the CoreNLP Server.
  *   Written by: Tom Hicks. 6/5/2017.
  *   Last Modified: Rename this object & file.
  */
object ProcessorCoreServerMessages {

  sealed trait ProcessorCoreServerMessage

  // message for request side of server communication:
  sealed trait ProcessorCoreCommand
  case class AnnotateCmd (doc:Document) extends ProcessorCoreCommand
  case class TagPartsOfSpeechCmd (doc:Document) extends ProcessorCoreCommand
  case class LemmatizeCmd (doc:Document) extends ProcessorCoreCommand
  case class RecognizeNamedEntitiesCmd (doc:Document) extends ProcessorCoreCommand
  case class ParseCmd (doc:Document) extends ProcessorCoreCommand
  case class ChunkingCmd (doc:Document) extends ProcessorCoreCommand
  case class LabelSemanticRolesCmd (doc:Document) extends ProcessorCoreCommand
  case class ResolveCoreferenceCmd (doc:Document) extends ProcessorCoreCommand
  case class DiscourseCmd (doc:Document) extends ProcessorCoreCommand


  // messages for response side of server communication:
  sealed trait ProcessorCoreReply
  case class TextMsg (text: String) extends ProcessorCoreReply


  // messages which are both commands and replies
  case class DocumentMsg (doc: Document) extends ProcessorCoreCommand with ProcessorCoreReply

}
