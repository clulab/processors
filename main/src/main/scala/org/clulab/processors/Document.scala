package org.clulab.processors

import org.clulab.discourse.rstparser.DiscourseTree
import org.clulab.struct.CorefChains
import org.clulab.utils.Serializer

import org.json4s.JString
import org.json4s.JValue
import org.json4s.jackson.prettyJson

import scala.collection.mutable
import scala.util.hashing.MurmurHash3._


/**
  * Stores all annotations for one document.
  *   Written by: Mihai Surdeanu and Gus Hahn-Powell.
  *   Last Modified: Add apply method to copy Document.
  */
class Document(val sentences: Array[Sentence]) extends Serializable {

  /** Unique id for this document, if any */
  var id: Option[String] = None

  /** Clusters of coreferent mentions */
  var coreferenceChains: Option[CorefChains] = None

  /** The RST discourse tree for this text */
  var discourseTree: Option[DiscourseTree] = None

  /** The original text corresponding to this document, if it was preserved by the corresponding processor */
  var text: Option[String] = None

  /** Map of any arbitrary document attachments such as document creation time */
  protected var attachments: Option[mutable.HashMap[String, DocumentAttachment]] = None

  /** Clears any internal state potentially constructed by the annotators */
  def clear() { }

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
      val h0 = stringHash(s"$stringCode.sentences")
      val hs = sentences.map(_.equivalenceHash)
      val h = mixLast(h0, unorderedHash(hs))
      finalizeHash(h, sentences.length)
    }

    // the seed (not counted in the length of finalizeHash)
    // decided to use the class name
    val h0 = stringHash(stringCode)
    // comprised of the equiv. hash of sentences
    val h1 = mix(h0, sentencesHash)
    finalizeHash(h1, 1)
  }

  def ambivalenceHash: Int = {
    val h0 = stringHash(Document.getClass.getName)
    val h1 = mix(h0, orderedHash(sentences.map(_.ambivalenceHash)))
    finalizeHash(h1, 1)
  }

  /** Adds an attachment to the document's attachment map */
  def addAttachment(name: String, attachment: DocumentAttachment): Unit = {
    if (attachments.isEmpty)
      attachments = Some(new mutable.HashMap[String, DocumentAttachment]())
    attachments.get += name -> attachment
  }

  /** Retrieves the attachment with the given name */
  def getAttachment(name: String): Option[DocumentAttachment] = {
    if (attachments.isEmpty) None
    else attachments.get.get(name)
  }

  /** Retrieves keys to all attachments so that the entire collection can be read
    * for purposes including but not limited to serialization.  If there are no
    * attachments, that is attachments == None, an empty set is returned.
    * This does not distinguish between None and Some(HashMap.empty), especially
    * since the latter should not be possible because of the lazy initialization.
    */
  def getAttachmentKeys: collection.Set[String] = {
    attachments.map { attachments =>
      attachments.keySet
    }.getOrElse(collection.Set.empty[String])
  }
}

object Document {

  def apply(sentences: Array[Sentence]): Document = new Document(sentences)

  def apply(id: Option[String], sentences: Array[Sentence], coref: Option[CorefChains], dtree: Option[DiscourseTree], text: Option[String]): Document = {
    val d = Document(sentences)
    d.id = id
    d.coreferenceChains = coref
    d.discourseTree = dtree
    d.text = text
    d
  }

  /** Return a new Document with relevant fields copied from the given Document. */
  def apply (doc: Document): Document =
    Document(doc.id, doc.sentences, doc.coreferenceChains, doc.discourseTree, doc.text)

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
    val byteArray: Array[Byte] = text.split(',').map { value =>
      val byte: Byte = java.lang.Byte.decode(value)
      byte
    }
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
    val byteArray: Array[Byte] = Serializer.save(this)
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
        val byteArray: Array[Byte] = text.split(',').map { value =>
          val byte: Byte = java.lang.Byte.decode(value)
          byte
        }
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
