package edu.arizona.sista.processors

import edu.arizona.sista.struct.Internalizer

/**
 * 
 * User: mihais
 * Date: 3/1/13
 */
trait Processor {
  /** Constructs a document of tokens from free text; includes sentence splitting and tokenization */
  def mkDocument(text:String, keepText:Boolean): Document

  /** Constructs a document of tokens from an array of untokenized sentences */
  def mkDocumentFromSentences(sentences:Iterable[String],
                              keepText:Boolean,
                              charactersBetweenSentences:Int = 1): Document

  /** Constructs a document of tokens from an array of tokenized sentences */
  def mkDocumentFromTokens(sentences:Iterable[Iterable[String]],
                           keepText:Boolean,
                           charactersBetweenSentences:Int = 1,
                           charactersBetweenTokens:Int = 1): Document

  /**
   * Part of speech tagging
   * This modifies the document in place, which is not too elegant. But there are two reasons for this:
   * (a) Some annotators (e.g., Stanford's CoreNLP) require some state (i.e., their Annotation object) to be passed between operations;
   * (b) This is more efficient during annotate() where all the possible operations are chained.
   */
  def tagPartsOfSpeech(doc:Document)

  /** Lematization; modifies the document in place */
  def lemmatize(doc:Document)

  /** NER; modifies the document in place */
  def recognizeNamedEntities(doc:Document)

  /** Syntactic parsing; modifies the document in place */
  def parse(doc:Document)

  /** Shallow parsing; modifies the document in place */
  def chunking(doc:Document)

  /** SRL; modifies the document in place */
  def labelSemanticRoles(doc:Document)

  /** Coreference resolution; modifies the document in place */
  def resolveCoreference(doc:Document)

  /** Discourse parsing; modifies the document in place */
  def discourse(doc:Document)

  def annotate(text:String, keepText:Boolean = false): Document = {
    val doc = mkDocument(text, keepText)
    annotate(doc)
  }

  def annotateFromSentences(sentences:Iterable[String], keepText:Boolean = false): Document = {
    val doc = mkDocumentFromSentences(sentences, keepText)
    annotate(doc)
  }

  def annotateFromTokens(sentences:Iterable[Iterable[String]], keepText:Boolean = false): Document = {
    val doc = mkDocumentFromTokens(sentences, keepText)
    annotate(doc)
  }

  def annotate(doc:Document): Document = {
    tagPartsOfSpeech(doc)
    lemmatize(doc)
    recognizeNamedEntities(doc)
    parse(doc)
    chunking(doc)
    labelSemanticRoles(doc)
    resolveCoreference(doc)
    discourse(doc)
    doc.clear()
    doc
  }
}

object Processor {
  /**
   * Used to (optionally) intern all the strings generated during annotation
   * This is now local to each thread to avoid concurrency issues in multi-threaded programs
   */
  private val in = new ThreadLocal[Internalizer[String]]

  def internString(s:String):String = {
    if(in.get() == null) in.set(new Internalizer[String])
    in.get.intern(s)
  }
}