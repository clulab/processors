package org.clulab.processors.csshare

import org.clulab.processors._

/**
  * Implement Akka message objects for the Processors Client/Server.
  *   Written by: Tom Hicks. 6/5/2017.
  *   Last Modified: Rename client/server packages and classes.
  */
object ProcessorCSMessages {

  // messages for request side of server communication:
  sealed trait ProcessorCSCommand

  case class ErrorTestCmd() extends ProcessorCSCommand

  case class AnnotateFromSentencesCmd (
    sentences:Iterable[String],
    keepText:Boolean = false
  ) extends ProcessorCSCommand

  case class AnnotateFromTokensCmd (
    sentences:Iterable[Iterable[String]],
    keepText:Boolean = false
  ) extends ProcessorCSCommand

  case class AnnotateTextCmd (text:String, keepText:Boolean = false) extends ProcessorCSCommand

//  case class AnnotateCmd (doc:Document) extends ProcessorCSCommand


  // messages for response side of server communication:
  sealed trait ProcessorCSReply

  case class ServerExceptionMsg (exception: Exception) extends ProcessorCSReply

  case class DocumentMsg (doc: Document) extends ProcessorCSReply
  case class SentencesMsg (sentences:Iterable[String]) extends ProcessorCSReply
  case class TextMsg (text:String) extends ProcessorCSReply
  case class TokensMsg (tokens:Iterable[Iterable[String]]) extends ProcessorCSReply
}
