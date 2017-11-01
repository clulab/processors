package org.clulab.processors

import org.clulab.struct.Internalizer

import scala.collection.mutable.ListBuffer

/**
  * User: mihais
  * Date: 3/1/13
  *   Last Modified: Move preprocess* methods from here to ProcessorAnnotator.
  */
trait Processor extends ProcessorAnnotator {

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


  // Side-effecting annotations. These modify the document in place, which is not too elegant.
  // There are two reasons for this:
  //   (1) Some annotators (e.g., Stanford's CoreNLP) require some state (i.e., their
  //       Annotation object) to be passed between operations,
  //   (2) It is more efficient during annotate() where all the possible operations are chained.

  /** Part of speech tagging; modifies the document in place. */
  def tagPartsOfSpeech (doc:Document)

  /** Lematization; modifies the document in place. */
  def lemmatize (doc:Document)

  /** Named Entity Recognition; modifies the document in place. */
  def recognizeNamedEntities (doc:Document)

  /** Syntactic parsing; modifies the document in place. */
  def parse (doc:Document)

  /** Shallow parsing; modifies the document in place. */
  def chunking (doc:Document)

  /** Coreference resolution; modifies the document in place. */
  def resolveCoreference (doc:Document)

  /** Discourse parsing; modifies the document in place. */
  def discourse (doc:Document)


  /** Annotate the given text string, specify whether to retain the text in the resultant Document. */
  override def annotate (text:String, keepText:Boolean = false): Document = {
    val doc = mkDocument(preprocessText(text), keepText)
    annotate(doc)
  }

  /** Annotate the given sentences, specify whether to retain the text in the resultant Document. */
  override def annotateFromSentences (
    sentences:Iterable[String],
    keepText:Boolean = false): Document =
  {
    val doc = mkDocumentFromSentences(preprocessSentences(sentences), keepText)
    annotate(doc)
  }

  /** Annotate the given tokens, specify whether to retain the text in the resultant Document. */
  override def annotateFromTokens (
    sentences:Iterable[Iterable[String]],
    keepText:Boolean = false): Document =
  {
    val doc = mkDocumentFromTokens(preprocessTokens(sentences), keepText)
    annotate(doc)
  }

  /**
    * Annotate the given document, returning an annotated document. The default implementation
    * is an NLP pipeline of side-effecting calls.
    */
  def annotate (doc:Document): Document = {
    tagPartsOfSpeech(doc)
    lemmatize(doc)
    recognizeNamedEntities(doc)
    parse(doc)
    chunking(doc)
    resolveCoreference(doc)
    discourse(doc)
    doc.clear()
    doc
  }
}


object Processor {
  /**
   * Used to (optionally) intern all the strings generated during annotation.
   * This is local to each thread to avoid concurrency issues in multi-threaded programs.
   */
  private val in = new ThreadLocal[Internalizer[String]]

  def internString (s:String): String = {
    if (in.get() == null)
      in.set(new Internalizer[String])
    in.get.intern(s)
  }
}
