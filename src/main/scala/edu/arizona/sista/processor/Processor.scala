package edu.arizona.sista.processor

import edu.arizona.sista.processor.struct.Internalizer

/**
 * 
 * User: mihais
 * Date: 3/1/13
 */
trait Processor {
  /** Constructs a document of tokens from free text; includes sentence splitting and tokenization */
  def mkDocument(text:String): Document

  /** Constructs a document of tokens from an array of untokenized sentences */
  def mkDocumentFromSentences(sentences:Iterable[String],
                              charactersBetweenSentences:Int = 1): Document

  /** Constructs a document of tokens from an array of tokenized sentences */
  def mkDocumentFromTokens(sentences:Iterable[Iterable[String]],
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

  def annotate(text:String): Document = {
    val doc = mkDocument(text)
    annotate(doc)
  }

  def annotateFromSentences(sentences:Iterable[String]): Document = {
    val doc = mkDocumentFromSentences(sentences)
    annotate(doc)
  }

  def annotateFromTokens(sentences:Iterable[Iterable[String]]): Document = {
    val doc = mkDocumentFromTokens(sentences)
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
    doc.clear()
    doc
  }
}

object Processor {
  /** Used to (optionally) intern all the strings generated during annotation */
  val in = new Internalizer[String]
}