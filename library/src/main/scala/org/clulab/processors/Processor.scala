package org.clulab.processors

import org.clulab.processors.clu.BalaurProcessor

import scala.collection.mutable

/**
  * User: mihais
  * Date: 3/1/13
  *   Last Modified: Move preprocess* methods from here to ProcessorAnnotator.
  */
trait Processor {

  /** Constructs a document of tokens from free text; includes sentence splitting and tokenization. */
  def mkDocument (text:String, keepText:Boolean = false): Document

  // The documents here were created with Processor.mkDocument, which could have created a subclassed
  // Document or documents with certain fields already filled in.  This implementation only handles
  // known document fields and then only performs rudimentary requirement checks to make sure that
  // the documents are compatible for combination.  In more complicated situations it would be necessary
  // to override this method in the Processor subclass.
  protected def combineDocuments(documents: IndexedSeq[Document], combinedTextOpt: Option[String]): Document = {
    require(documents.length > 1)
    val headDocument = documents.head
    val tailDocuments = documents.tail

    val headId = headDocument.id
    require(tailDocuments.forall(_.id == headId))
    val headDctOpt = headDocument.getDCT
    require(documents.tail.forall(_.getDCT == headDctOpt))
    // Coreference chains involve Mentions that include references to documents.  The Mentions are being
    // moved to a new Document and it would be infeasible to move the chains.
    require(documents.forall(_.coreferenceChains.isEmpty))

    val attachments = mutable.HashMap[String, DocumentAttachment]()

    documents.foreach { document =>
      document.getAttachmentKeys.foreach { attachmentKey =>
        val valueOpt = attachments.get(attachmentKey)
        val isValid = valueOpt.forall(_ == document.getAttachment(attachmentKey).get)

        require(isValid, "The attachments cannot contradict each other.")
        attachments(attachmentKey) = document.getAttachment(attachmentKey).get
      }
    }

    val combinedSentences = documents.flatMap(_.sentences).toArray
    val combinedDocument = new Document(
      sentences = combinedSentences,
      id = headId,
      coreferenceChains = None,
      text = combinedTextOpt,
      attachments = Some(attachments),
      documentCreationTime = headDctOpt
    )

    combinedDocument
  }

  def mkCombinedDocument(texts: IndexedSeq[String], trailers: IndexedSeq[String], keepText: Boolean = false): Document = {
    require(texts.length == trailers.length)
    texts.length match {
      case 0 => mkDocument("", keepText)
      case 1 => mkDocument(texts.head, keepText)
      case _ =>
        val documents = texts.map(mkDocument(_, keepText))
        val offsets = texts.zip(trailers).scanLeft(0) { case (offset, (text, trailer)) => offset + text.length + trailer.length }
        val offsetDocuments = documents.zip(offsets).map { case (document, offset) =>
          document.offset(offset)
        }
        val combinedTextOpt =
            if (keepText) {
              val combinedText = texts.zip(trailers).foldLeft(new StringBuilder) { case (stringBuilder, (text, separator)) =>
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
  def tagPartsOfSpeech(doc: Document): Unit

  /** Lemmatization; modifies the document in place. */
  def lemmatize(words: Array[String]): Array[String]

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
  def apply(): Processor = {
    new BalaurProcessor()
  }

  def mkProcessor(): Processor = {
    apply()
  }
}
