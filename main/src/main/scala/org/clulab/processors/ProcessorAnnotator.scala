package org.clulab.processors

import scala.collection.breakOut

import org.clulab.processors.csshare.ProcessorCSController

/**
  * Common interface for accessing either CLU Lab or CoreNLP processors.
  *   Written by: Tom Hicks. 10/3/2017.
  *   Last Modified: Let Scala efficiently determine conversion return types.
  */
trait ProcessorAnnotator extends ProcessorCSController {

  /** Annotate the given text string, specify whether to retain the text in the resultant Document. */
  def annotate (text:String, keepText:Boolean = false): Document

  /** Annotate the given sentences, specify whether to retain the text in the resultant Document. */
  def annotateFromSentences (sentences:Iterable[String], keepText:Boolean = false): Document

  /** Annotate the given tokens, specify whether to retain the text in the resultant Document. */
  def annotateFromTokens (sentences:Iterable[Iterable[String]], keepText:Boolean = false): Document

}
