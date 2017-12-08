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


  /**
    * Hook to allow the preprocessing of input text. This is useful for domain-specific
    * corrections, such as the ones in BioNLPProcessor, where we remove Table and Fig references.
    * Note that this is allowed to change character offsets.
    *   @param origText The original input text
    *   @return The preprocessed text
    */
  def preprocessText (origText:String): String = origText

  /** Runs preprocessText on each sentence. */
  def preprocessSentences (origSentences:Iterable[String]): Iterable[String] = {
    origSentences.map(os => preprocessText(os))(breakOut)
  }

  /** Runs preprocessText on each token. */
  def preprocessTokens (origSentences:Iterable[Iterable[String]]): Iterable[Iterable[String]] = {
    origSentences.map { os =>
      os.map(token => preprocessText(token))(breakOut)
    }(collection.breakOut)
  }

}
