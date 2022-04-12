package org.clulab.processors

import java.io.PrintWriter

import org.clulab.struct.{CorefChains, DirectedGraphEdgeIterator}
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

  /** The original text corresponding to this document, if it was preserved by the corresponding processor */
  var text: Option[String] = None

  /** Map of any arbitrary document attachments such as document creation time */
  protected var attachments: Option[mutable.HashMap[String, DocumentAttachment]] = None

  protected var documentCreationTime:Option[String] = None

  /** Clears any internal state potentially constructed by the annotators */
  def clear(): Unit = { }

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
    attachments.flatMap(_.get(name))
  }

  def removeAttachment(name: String): Unit = {
    if(attachments.nonEmpty) {
      attachments.get -= name
    }
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

  /**
   * Sets the document creation time using the CoreNLP format.
   * See useFixedDate here for more details: https://stanfordnlp.github.io/CoreNLP/ner.html#setting-document-date
   * The DCT will impacts how Sentence.norms are generated for DATE expressions
   * @param dct Document creation time
   */
  def setDCT(dct:String): Unit = {
    documentCreationTime = Some(dct)
  }

  def getDCT: Option[String] = documentCreationTime

  def prettyPrint(pw: PrintWriter): Unit = {
    // let's print the sentence-level annotations
    var sentenceCount = 0
    for (sentence <- sentences) {
      pw.println("Sentence #" + sentenceCount + ":")
      pw.println("Tokens: " + sentence.words.zipWithIndex.mkString(" "))
      pw.println("Start character offsets: " + sentence.startOffsets.mkString(" "))
      pw.println("End character offsets: " + sentence.endOffsets.mkString(" "))

      // these annotations are optional, so they are stored using Option objects, hence the foreach statement
      sentence.lemmas.foreach(lemmas => pw.println(s"Lemmas: ${lemmas.mkString(" ")}"))
      sentence.tags.foreach(tags => pw.println(s"POS tags: ${tags.mkString(" ")}"))
      sentence.chunks.foreach(chunks => pw.println(s"Chunks: ${chunks.mkString(" ")}"))
      sentence.entities.foreach(entities => pw.println(s"Named entities: ${entities.mkString(" ")}"))
      sentence.norms.foreach(norms => pw.println(s"Normalized entities: ${norms.mkString(" ")}"))
      sentence.universalBasicDependencies.foreach(dependencies => {
        pw.println("Basic syntactic dependencies:")
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        while(iterator.hasNext) {
          val dep = iterator.next()
          // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
          pw.println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
        }
      })
      sentence.universalEnhancedDependencies.foreach(dependencies => {
        pw.println("Enhanced syntactic dependencies:")
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        while(iterator.hasNext) {
          val dep = iterator.next()
          // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
          pw.println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
        }
      })
      sentence.semanticRoles.foreach(dependencies => {
        pw.println("Semantic dependencies:")
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        while(iterator.hasNext) {
          val dep = iterator.next()
          // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
          pw.println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
        }
      })
      sentence.enhancedSemanticRoles.foreach(dependencies => {
        pw.println("Enhanced semantic dependencies:")
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        while(iterator.hasNext) {
          val dep = iterator.next()
          // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
          pw.println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
        }
      })
      sentence.syntacticTree.foreach(tree => {
        pw.println("Constituent tree: " + tree.toStringDepth(showHead = false))
        // see the org.clulab.struct.Tree class for more information
        // on syntactic trees, including access to head phrases/words
      })

      sentenceCount += 1
      pw.println("\n")
    }

    // let's print the coreference chains
    coreferenceChains.foreach(chains => {
      for (chain <- chains.getChains) {
        pw.println("Found one coreference chain containing the following mentions:")
        for (mention <- chain) {
          // note that all these offsets start at 0 too
          pw.println("\tsentenceIndex:" + mention.sentenceIndex +
            " headIndex:" + mention.headIndex +
            " startTokenOffset:" + mention.startOffset +
            " endTokenOffset:" + mention.endOffset +
            " text: " + sentences(mention.sentenceIndex).words.slice(mention.startOffset, mention.endOffset).mkString("[", " ", "]"))
        }
      }
    })

  }
}

object Document {

  def apply(sentences: Array[Sentence]): Document = new Document(sentences)

  def apply(id: Option[String], sentences: Array[Sentence], coref: Option[CorefChains], text: Option[String]): Document = {
    val d = Document(sentences)
    d.id = id
    d.coreferenceChains = coref
    d.text = text
    d
  }

  /** Return a new Document with relevant fields copied from the given Document. */
  def apply (doc: Document): Document =
    Document(doc.id, doc.sentences, doc.coreferenceChains, doc.text)

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
