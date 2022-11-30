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

  protected def offsetSentence(sentence: Sentence, charOffset: Int): Sentence = {
    val raw = sentence.raw
    val startOffsets = sentence.startOffsets.map(_ + charOffset)
    val endOffsets = sentence.endOffsets.map(_ + charOffset)
    val words = sentence.words
    val newSentence = Sentence(raw, startOffsets, endOffsets, words)

    newSentence.tags = sentence.tags
    newSentence.lemmas = sentence.lemmas
    newSentence.entities = sentence.entities
    newSentence.norms = sentence.norms
    newSentence.chunks = sentence.chunks
    newSentence.syntacticTree = sentence.syntacticTree
    newSentence.graphs = sentence.graphs
    newSentence.relations = sentence.relations
    newSentence
  }

  protected def offsetDocument(document: Document, offset: Int): Document = {
    if (offset == 0) document
    else {
      val offsetSentences = document.sentences.map(offsetSentence(_, offset))
      val newDocument = replaceSentences(document, offsetSentences)

      newDocument
    }
  }

  protected def replaceSentences(document: Document, sentences: Array[Sentence]): Document = {
    val newDocument = new Document(sentences)

    newDocument.id = document.id
    newDocument.text = document.text

    require(newDocument.coreferenceChains.isEmpty)
    require(document.coreferenceChains.isEmpty)

    document.getAttachmentKeys.foreach { attachmentKey =>
      require(newDocument.getAttachment(attachmentKey).forall(_ == document.getAttachment(attachmentKey).get))
      newDocument.addAttachment(attachmentKey, document.getAttachment(attachmentKey).get)
    }

    val dctOpt = document.getDCT
    dctOpt.foreach(newDocument.setDCT)

    newDocument
  }

  // The documents here were created with Processor.mkDocument, which could have created a subclassed
  // Document or documents with certain fields already filled in.  This implementation only handles
  // known document fields and then only performs rudimentary requirement checks to make sure that
  // the documents are compatible for combination.  In more complicated situations it would be necessary
  // to override this method in the Processor subclass.
  protected def combineDocuments(documents: IndexedSeq[Document], combinedTextOpt: Option[String]): Document = {
    require(documents.length > 1)
    val headDocument = documents.head
    val tailDocuments = documents.tail
    val combinedSentences = documents.flatMap(_.sentences).toArray
    val combinedDocument = new Document(combinedSentences)

    val headId = headDocument.id
    require(tailDocuments.forall(_.id == headId))
    combinedDocument.id = headId

    require(combinedDocument.text.isEmpty)
    combinedDocument.text = combinedTextOpt

    // Coreference chains involve Mentions that include references to documents.  The Mentions are being
    // moved to a new Document and it would be infeasible to move the chains.
    require(combinedDocument.coreferenceChains.isEmpty)
    require(documents.forall(_.coreferenceChains.isEmpty))

    documents.foreach { document =>
      document.getAttachmentKeys.foreach { attachmentKey =>
        require(combinedDocument.getAttachment(attachmentKey).forall(_ == document.getAttachment(attachmentKey).get))
        combinedDocument.addAttachment(attachmentKey, document.getAttachment(attachmentKey).get)
      }
    }

    val headDctOpt = headDocument.getDCT
    require(documents.tail.forall(_.getDCT == headDctOpt))
    headDctOpt.foreach(combinedDocument.setDCT)
    combinedDocument
  }

  def mkCombinedDocument(texts: IndexedSeq[String], separators: IndexedSeq[String], keepText: Boolean = false): Document = {
    require(texts.length == separators.length)
    texts.length match {
      case 0 => mkDocument("", keepText)
      case 1 => mkDocument(texts.head, keepText)
      case _ =>
        val documents = texts.map(mkDocument(_, keepText))
        val offsets = texts.zip(separators).scanLeft(0) { case (offset, (text, separator)) => offset + text.length + separator.length }
        val offsetDocuments = documents.zip(offsets).map { case (document, offset) =>
          offsetDocument(document, offset) // charOffset and wordOffset, because some things are counted in words?
        }
        val combinedTextOpt =
            if (keepText) {
              val combinedText = texts.zip(separators).foldLeft(new StringBuilder) { case (stringBuilder, (text, separator)) =>
                stringBuilder.append(text).append(separator)
              }.toString

              Some(combinedText)
            }
            else None
        val combinedDocument = combineDocuments(offsetDocuments, combinedTextOpt)

        combinedDocument
    }
  }

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
  def annotate(doc: Document): Document
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

  def clearStrings(): Unit = in.get.clear()
}
