package org.clulab.processors

import org.clulab.struct.Internalizer

/**
  * User: mihais
  * Date: 3/1/13
  *   Last Modified: Move preprocess* methods from here to ProcessorAnnotator.
  */
trait Processor {

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
  def tagPartsOfSpeech (doc:Document): Unit

  /** Lematization; modifies the document in place. */
  def lemmatize (doc:Document): Unit

  /** Named Entity Recognition; modifies the document in place. */
  def recognizeNamedEntities (doc:Document): Unit

  /** Syntactic parsing; modifies the document in place. */
  def parse (doc:Document): Unit

  /** Semantic role labeling */
  def srl (doc: Document): Unit

  /** Shallow parsing; modifies the document in place. */
  def chunking (doc:Document): Unit

  /** Coreference resolution; modifies the document in place. */
  def resolveCoreference (doc:Document): Unit

  /** Discourse parsing; modifies the document in place. */
  def discourse (doc:Document): Unit

  /** Relation extraction; modifies the document in place. */
  def relationExtraction(doc:Document): Unit


  /** Annotate the given text string, specify whether to retain the text in the resultant Document. */
  def annotate (text:String, keepText:Boolean = false): Document = {
    val doc = mkDocument(text, keepText)
    if (doc.sentences.nonEmpty)
      annotate(doc)
    else
      doc
  }

  /** Annotate the given sentences, specify whether to retain the text in the resultant Document. */
  def annotateFromSentences (
    sentences:Iterable[String],
    keepText:Boolean = false): Document = {
    val doc = mkDocumentFromSentences(sentences, keepText)
    annotate(doc)
  }

  /** Annotate the given tokens, specify whether to retain the text in the resultant Document. */
  def annotateFromTokens (
    sentences:Iterable[Iterable[String]],
    keepText:Boolean = false): Document = {
    val doc = mkDocumentFromTokens(sentences, keepText)
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
    relationExtraction(doc)
    srl(doc)
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
