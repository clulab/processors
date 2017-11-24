package org.clulab.processors.csshare

import org.clulab.processors._

/**
  * Implement Akka message objects for the Processors Client/Server.
  *   Written by: Tom Hicks. 6/5/2017.
  *   Last Modified: Add default serializer encoding constant.
  */
object ProcessorCSMessages {

  // Specify UTF-8 encoding for serializer, which, sadly, still uses archaic Latin 1 char set
  val SERIALIZER_ENCODING = "UTF-8"

  // Messages for request side of server communication:
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

  case class PreprocessTextCmd (text:String) extends ProcessorCSCommand
  case class PreprocessSentencesCmd (sentences:Iterable[String]) extends ProcessorCSCommand
  case class PreprocessTokensCmd (sentences:Iterable[Iterable[String]]) extends ProcessorCSCommand


  // Messages for response side of server communication:
  sealed trait ProcessorCSReply

  case class ServerExceptionMsg (exception: Exception) extends ProcessorCSReply

  case class DocumentMsg (doc: Document) extends ProcessorCSReply
  case class SentencesMsg (sentences:Iterable[String]) extends ProcessorCSReply
  case class TextMsg (text:String) extends ProcessorCSReply
  case class TokensMsg (tokens:Iterable[Iterable[String]]) extends ProcessorCSReply
}
