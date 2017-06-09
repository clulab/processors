package org.clulab.processors.coserver

import org.clulab.processors._

/**
  * Implement Akka message objects for the CoreNLP Server.
  *   Written by: Tom Hicks. 6/5/2017.
  *   Last Modified: Move coserver package.
  */
object ProcessorCoreServerMessages {

  sealed trait ProcessorCoreServerMessage

  // message for request side of server communication:
  sealed trait ProcessorCoreCommand

  case class MkDocumentCmd (text:String, keepText:Boolean = false) extends ProcessorCoreCommand

  case class MkDocumentFromSentencesCmd (
    sentences:Iterable[String],
    keepText:Boolean = false,
    charactersBetweenSentences:Int = 1
  ) extends ProcessorCoreCommand

  case class MkDocumentFromTokensCmd (
    sentences:Iterable[Iterable[String]],
    keepText:Boolean = false,
    charactersBetweenSentences:Int = 1,
    charactersBetweenTokens:Int = 1
  ) extends ProcessorCoreCommand

  case class PreprocessTextCmd (origText:String) extends ProcessorCoreCommand
  case class PreprocessSentencesCmd (origSentences:Iterable[String]) extends ProcessorCoreCommand
  case class PreprocessTokensCmd (origSentences:Iterable[Iterable[String]]) extends ProcessorCoreCommand

  case class TagPartsOfSpeechCmd (doc:Document) extends ProcessorCoreCommand
  case class LemmatizeCmd (doc:Document) extends ProcessorCoreCommand
  case class RecognizeNamedEntitiesCmd (doc:Document) extends ProcessorCoreCommand
  case class ParseCmd (doc:Document) extends ProcessorCoreCommand
  case class ChunkingCmd (doc:Document) extends ProcessorCoreCommand
  case class LabelSemanticRolesCmd (doc:Document) extends ProcessorCoreCommand
  case class ResolveCoreferenceCmd (doc:Document) extends ProcessorCoreCommand
  case class DiscourseCmd (doc:Document) extends ProcessorCoreCommand

  case class AnnotateFromSentencesCmd (
    sentences:Iterable[String],
    keepText:Boolean = false
  ) extends ProcessorCoreCommand

  case class AnnotateFromTokensCmd (
    sentences:Iterable[Iterable[String]],
    keepText:Boolean = false
  ) extends ProcessorCoreCommand

  case class AnnotateStringCmd (text:String, keepText:Boolean = false) extends ProcessorCoreCommand
  case class AnnotateCmd (doc:Document) extends ProcessorCoreCommand


  // messages for response side of server communication:
  sealed trait ProcessorCoreReply
  case class TextMsg (text: String) extends ProcessorCoreReply


  // messages which are both commands and replies
  case class DocumentMsg (doc: Document) extends ProcessorCoreCommand with ProcessorCoreReply

}
