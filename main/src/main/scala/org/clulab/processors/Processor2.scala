package org.clulab.processors

import org.clulab.struct.Internalizer

import scala.collection.mutable.ListBuffer

/**
  * Common interface for accessing either CLU Lab or CoreNLP processors.
  *   Written by: Tom Hicks after original processor trait by Mihai Surdeanu. 10/3/2017.
  *   Last Modified: Initial creation by altering Processors to remove side-effecting methods.
  */
trait Processor2 {

  /** Constructs a document of tokens from free text; includes sentence splitting and tokenization. */
  def mkDocument (text:String, keepText:Boolean = false): Document

  /** Constructs a document of tokens from an array of untokenized sentences. */
  def mkDocumentFromSentences (sentences:Iterable[String],
                               keepText:Boolean = false,
                               charactersBetweenSentences:Int = 1): Document

  /** Constructs a document of tokens from an array of tokenized sentences. */
  def mkDocumentFromTokens (sentences:Iterable[Iterable[String]],
                            keepText:Boolean = false,
                            charactersBetweenSentences:Int = 1,
                            charactersBetweenTokens:Int = 1): Document

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
    val sents = new ListBuffer[String]()
    for (os <- origSentences)
      sents += preprocessText(os)
    sents.toList
  }

  /** Runs preprocessText on each token. */
  def preprocessTokens (origSentences:Iterable[Iterable[String]]): Iterable[Iterable[String]] = {
    val sents = new ListBuffer[Iterable[String]]
    for (origSentence <- origSentences) {
      val sent = new ListBuffer[String]
      for (origToken <- origSentence) {
        sent += preprocessText(origToken)
      }
      sents += sent.toList
    }
    sents.toList
  }


  //
  // The following operations from Processors are redefined here because they use
  // modification in place, which is not compatible with messaging implementations.
  //

  /** Part of speech tagging. */
  def tagPartsOfSpeech (doc:Document): Document

  /** Lematization. */
  def lemmatize (doc:Document): Document

  /** Named Entity Recognition. */
  def recognizeNamedEntities (doc:Document): Document

  /** Syntactic parsing. */
  def parse (doc:Document): Document

  /** Shallow parsing. */
  def chunking (doc:Document): Document

  /** Coreference resolution. */
  def resolveCoreference (doc:Document): Document

  /** Discourse parsing. */
  def discourse (doc:Document): Document


  /** Annotate the given text string, specify whether to retain the text in the resultant Document. */
  def annotate (text:String, keepText:Boolean = false): Document = {
    val doc = mkDocument(preprocessText(text), keepText)
    annotate(doc)
  }

  /** Annotate the given sentences, specify whether to retain the text in the resultant Document. */
  def annotateFromSentences (sentences:Iterable[String], keepText:Boolean = false): Document = {
    val doc = mkDocumentFromSentences(preprocessSentences(sentences), keepText)
    annotate(doc)
  }

  /** Annotate the given tokens, specify whether to retain the text in the resultant Document. */
  def annotateFromTokens (
    sentences:Iterable[Iterable[String]],
    keepText:Boolean = false): Document =
  {
    val doc = mkDocumentFromTokens(preprocessTokens(sentences), keepText)
    annotate(doc)
  }

  /** Annotate the given document, returning an annotated document. The default implementation
    * is an NLP pipeline (as a nested series of calls) but trait implementors might
    * consider overriding this method for a more efficient implementation.
    */
  def annotate (doc:Document): Document = {
    val newdoc =
      discourse(
        resolveCoreference(
          chunking(
            parse(
              recognizeNamedEntities(
                lemmatize(
                  tagPartsOfSpeech(doc) ))))))
    newdoc.clear()
    newdoc
  }
}
