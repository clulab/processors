package org.clulab.processors.coshare

import org.clulab.processors._

/**
  * Implement Akka message objects for the CoreNLP Server.
  *   Written by: Tom Hicks. 6/5/2017.
  *   Last Modified: Update to implement processor annotator trait only.
  */
object ProcessorCoreMessages {

  // messages for request side of server communication:
  sealed trait ProcessorCoreCommand

  case class ErrorTestCmd() extends ProcessorCoreCommand

  case class AnnotateFromSentencesCmd (
    sentences:Iterable[String],
    keepText:Boolean = false
  ) extends ProcessorCoreCommand

  case class AnnotateFromTokensCmd (
    sentences:Iterable[Iterable[String]],
    keepText:Boolean = false
  ) extends ProcessorCoreCommand

  case class AnnotateTextCmd (text:String, keepText:Boolean = false) extends ProcessorCoreCommand

//  case class AnnotateCmd (doc:Document) extends ProcessorCoreCommand


  // messages for response side of server communication:
  sealed trait ProcessorCoreReply

  case class ServerExceptionMsg (exception: Exception) extends ProcessorCoreReply

  case class DocumentMsg (doc: Document) extends ProcessorCoreReply
  case class SentencesMsg (sentences:Iterable[String]) extends ProcessorCoreReply
  case class TextMsg (text:String) extends ProcessorCoreReply
  case class TokensMsg (tokens:Iterable[Iterable[String]]) extends ProcessorCoreReply
}
