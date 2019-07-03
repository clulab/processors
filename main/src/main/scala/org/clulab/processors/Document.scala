package org.clulab.processors

import org.clulab.discourse.rstparser.DiscourseTree
import org.clulab.struct.CorefChains

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
  protected var attachments:Option[mutable.HashMap[String, DocumentAttachment]] = None

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
  def addAttachment(name:String, attachment: DocumentAttachment): Unit = {
    if(attachments.isEmpty)
      attachments = Some(new mutable.HashMap[String, DocumentAttachment]())
    attachments.get += name -> attachment
  }

  /** Retrieves the attachment with the given name */
  def getAttachment(name:String):Option[DocumentAttachment] = {
    if(attachments.isEmpty) None
    else attachments.get.get(name)
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
  * Placeholder for document attachment, to be used to store any meta data such as document creation time
  */
trait DocumentAttachment {
  // TODO: add trait methods for the serialization of attachments (are these needed here?)
}