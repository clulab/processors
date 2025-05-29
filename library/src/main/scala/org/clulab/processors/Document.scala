package org.clulab.processors

import org.clulab.struct.{CorefChains, DirectedGraphEdgeIterator}
import org.clulab.utils.Hash
import org.clulab.utils.Serializer
import org.json4s.JString
import org.json4s.JValue
import org.json4s.jackson.prettyJson

import java.io.PrintWriter

/**
  * Stores all annotations for one document.
  *   Written by: Mihai Surdeanu and Gus Hahn-Powell.
  *   Last Modified: Add apply method to copy Document.
  */
class Document(
  val sentences: Seq[Sentence],
  /** Unique id for this document, if any */
  val id: Option[String] = None,
  /** Clusters of coreferent mentions */
  val coreferenceChains: Option[CorefChains] = None,
  /** The original text corresponding to this document, if it was preserved by the corresponding processor */
  val text: Option[String] = None,
  /** Map of any arbitrary document attachments such as document creation time */
  val attachments: Option[DocumentAttachments.Type] = None,
  /**
   * The document creation time using the CoreNLP format
   * See useFixedDate here for more details: https://stanfordnlp.github.io/CoreNLP/ner.html#setting-document-date
   * The DCT will impact how Sentence.norms are generated for DATE expressions.
   */
  val dct: Option[String] = None
) extends Serializable {

  def copy(
    sentences: Seq[Sentence] = sentences,
    id: Option[String] = id,
    coreferenceChains: Option[CorefChains] = coreferenceChains,
    text: Option[String] = text,
    attachments: Option[DocumentAttachments.Type] = None,
    dct: Option[String] = dct
  ): Document = new Document(sentences, id, coreferenceChains, text, attachments, dct)

  /** Clears any internal state potentially constructed by the annotators */
  def clear(): Unit = { } // This is for subclass support.

  /**
    * Used to compare Documents.
    * @return a hash (Int) based primarily on the sentences, ignoring attachments
    */
  def equivalenceHash: Int = {
    val stringCode = "org.clulab.processors.Document"

    // Hash representing the sentences.
    // Used by equivalenceHash.
    // return an Int hash based on the Sentence.equivalenceHash of each sentence
    def sentencesHash: Int = {
      val hs = sentences.map(_.equivalenceHash)

      Hash.withLast(sentences.length)(
        Hash(s"$stringCode.sentences"),
        Hash.unordered(hs) // TODO: This should be ordered.
      )
    }

    Hash(
      Hash(stringCode),
      sentencesHash
    )
  }

  def ambivalenceHash: Int = Hash(
    Hash(Document.getClass.getName),
    Hash.ordered(sentences.map(_.ambivalenceHash))
  )

  def offset(offset: Int): Document =
      // If a subclass of Document constructs itself with an attachment or a documentCreationTime that
      // would be overwritten on the copy(), then it should provide its own copy() method(s).
      if (offset == 0) this
      else copy(sentences = sentences.map(_.offset(offset)))
}

object Document {

  def apply(sentences: Seq[Sentence]): Document = apply(sentences, text = None)

  def apply(sentences: Seq[Sentence], text: Option[String]): Document = apply(id = None, sentences, coref = None, text)

  def apply(id: Option[String], sentences: Seq[Sentence], coref: Option[CorefChains], text: Option[String]): Document = {
    val document = new Document(
      sentences,
      id = id,
      coreferenceChains = coref,
      text = text
    )

    document
  }

  /** Return a new Document with some relevant fields copied from the given Document. */
  def apply(doc: Document): Document =
    apply(doc.id, doc.sentences, doc.coreferenceChains, doc.text)

  def apply(doc: Document, sentences: Seq[Sentence]): Document = {
    val newDocument = new Document(
      sentences,
      id = doc.id,
      coreferenceChains = doc.coreferenceChains,
      text = doc.text,
      attachments = doc.attachments,
      dct = doc.dct
    )

    newDocument
  }
}

/**
  * This is the interface required for interaction with Document
  * Since the Document is serializable, everything in it should be, including the DocumentAttachments.
  */
trait DocumentAble extends Serializable

/**
  * This is the interface used to build DocumentAttachments from text when
  * they are deserialized by the DocumentSerializer as coded further below.
  */
trait DocumentAttachmentBuilderFromText {
  def mkDocumentAttachment(text: String): DocumentAttachment
}

/**
  * This is an implementation of DocumentAttachmentBuilderFromText that indeed
  * creates an object from text, but it is an illegible text version of the Java
  * object serialization.  It is used as a backup absent a more legible representation.
  * Subclasses must have an empty constructor which can be called using reflection
  * based on the name in DocumentAttachment.documentAttachmentBuilderClassName by
  * the DocumentSerializer.  The constructed builder is then used to construct the
  * DocumentAttachment by calling mkDocumentAttachment which likely returns a
  * customized subclass of DocumentAttachment.
  */
class ObjectDocumentAttachmentBuilderFromText extends DocumentAttachmentBuilderFromText {

  /**
    * This matches the format in DocumentAttachment.toDocumentSerializer.
    */
  def mkDocumentAttachment(text: String): DocumentAttachment = {
    val byteArray = text.split(',').map(java.lang.Byte.decode(_).asInstanceOf[Byte])
    val documentAttachment: DocumentAttachment = Serializer.load(byteArray)
    documentAttachment
  }
}

/**
  * These are interfaces required for interaction with the DocumentSerializer.
  */
trait DocumentSerializerAble {
  /**
    * An object of this class will (re)construct the DocumentAttachment (or subclass thereof).
    */
  def documentAttachmentBuilderFromTextClassName: String = classOf[ObjectDocumentAttachmentBuilderFromText].getName

  /** A DocumentSerializer needs/wants to convert the attachment into a compact, legible string.
    * The default implementation for use in an emergency just converts the Java ObjectOutputStream
    * into a string.  A good implementation of the trait will override this behavior.
    */
  def toDocumentSerializer: String = {
    val byteArray = Serializer.save(this)
    byteArray.mkString(",")
  }
}

/**
  * This is the interface used to build DocumentAttachments from json when
  * they are deserialized by the JSONSerializer as coded further below.
  */
trait DocumentAttachmentBuilderFromJson {
  def mkDocumentAttachment(json: JValue): DocumentAttachment
}

/**
  * This design parallels that of ObjectDocumentAttachmentBuilderFromJson.
  */
class ObjectDocumentAttachmentBuilderFromJson extends DocumentAttachmentBuilderFromJson {

  def mkDocumentAttachment(json: JValue): DocumentAttachment = {
    json match {
      case JString(text) =>
        val byteArray = text.split(',').map(java.lang.Byte.decode(_).asInstanceOf[Byte])
        val documentAttachment: DocumentAttachment = Serializer.load(byteArray)
        documentAttachment
      case _ =>
        val text = prettyJson(json)
        throw new RuntimeException(s"ERROR: While deserializing document attachment expected JString but found this: $text")
    }
  }
}

/**
  * These are interfaces required for interaction with JSONSerializer (and related classes).
  * See DocumentSerializerAble for a similar implementation but for the text representation.
  */
trait JsonSerializerAble {
  def documentAttachmentBuilderFromJsonClassName: String = classOf[ObjectDocumentAttachmentBuilderFromJson].getName

  def toJsonSerializer: JValue = {
    val byteArray: Array[Byte] = Serializer.save(this)
    new JString(byteArray.mkString(","))
  }
}

/**
  * Placeholder for document attachment, to be used to store any meta data such as document creation time.
  */
trait DocumentAttachment extends DocumentAble with DocumentSerializerAble with JsonSerializerAble

object DocumentAttachments {
  type Type = Map[String, DocumentAttachment]
}


/**
 * Designed to store intermediate attachments that are only used to pass information between processor components.
 * Thus, these do not need to be serialized
 */
class IntermediateDocumentAttachment extends DocumentAttachment {
  override def toDocumentSerializer: String = ""

  override def toJsonSerializer: JValue = {
    new JString("")
  }
}
