package org.clulab.serialization.json

import java.io.File
import org.clulab.processors.{Document, DocumentAttachment, DocumentAttachmentBuilderFromJson, Sentence}
import org.clulab.struct.Edge
import org.clulab.struct.{DirectedGraph, GraphMap}
import org.clulab.utils.FileUtils
import org.json4s
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.prettyJson

import scala.collection.mutable


/** JSON serialization utilities */
// This annotation is to avoid "Compiler synthesis of Manifest and OptManifest is deprecated".
@annotation.nowarn("cat=deprecation")
object JSONSerializer {
  implicit val formats: DefaultFormats.type = DefaultFormats

  def jsonAST(s: String): JValue = parse(s)

  def jsonAST(f: File): JValue = jsonAST(FileUtils.getTextFromFile(f))

  protected def getDocumentAttachments(jValue: JValue): Option[mutable.HashMap[String, DocumentAttachment]] = {
    // See also DocumentSerializer for text version of nearly the same thing.
    (jValue \ DOCUMENT_ATTACHMENTS_KEY) match {
      case jObject: JObject =>
        val attachments = new mutable.HashMap[String, DocumentAttachment]()
        val keys = jObject.values.keys
        keys.foreach { (key: String) =>
          (jObject \ key) match {
            case jObject: JObject =>
              val documentAttachmentBuilderFromJsonClassName = (jObject \ DOCUMENT_ATTACHMENTS_BUILDER_KEY).extract[String]
              val clazz = Class.forName(documentAttachmentBuilderFromJsonClassName)
              val ctor = clazz.getConstructor()
              val obj = ctor.newInstance()
              val documentAttachmentBuilder = obj.asInstanceOf[DocumentAttachmentBuilderFromJson]
              val value = (jObject \ DOCUMENT_ATTACHMENTS_VALUE_KEY)
              val documentAttachment = documentAttachmentBuilder.mkDocumentAttachment(value)
              attachments(key) = documentAttachment
            case jValue: JValue =>
              val text = prettyJson(jValue)
              throw new RuntimeException(s"ERROR: While deserializing document attachments expected JObject but found this: $text")
            // case _ => // noop.  It should never get here.  (Famous last words.)
            case null => // noop.  It should never get here.  (Famous last words.)  Scala 3 prefers null over _.
          }
        }
        Some(attachments)
      case _ => // Leave documentAttachments as is: None
        None
    }
  }

  def toDocument(json: JValue): Document = {
    // recover sentences
    val sentences = (json \ "sentences").asInstanceOf[JArray].arr.map(sjson => toSentence(sjson)).toArray
    val id = getStringOption(json, "id")
    val text = getStringOption(json, "text")
    // initialize document
    val attachments = getDocumentAttachments(json)
    val d = new Document(
      id = id,
      sentences = sentences,
      coreferenceChains = None,
      text = text,
      attachments = attachments
    )

    d
  }
  def toDocument(docHash: String, djson: JValue): Document = toDocument(djson \ docHash)
  def toDocument(f: File): Document = toDocument(jsonAST(f))
  def toDocument(s: String): Document = toDocument(jsonAST(s))

  def toSentence(json: JValue): Sentence = {

    def getStrings(json: JValue, k: String): Array[String] = (json \ k).extract[Array[String]]

    def getInts(json: JValue, k: String): Array[Int] = (json \ k).extract[Array[Int]]

    def getLabelsOpt(json: JValue, k: String): Option[Seq[String]] = json \ k match {
      case JNothing => None
      case contents => Some(contents.extract[Array[String]])
    }

    val raw = getStrings(json, "raw")
    val startOffsets = getInts(json, "startOffsets")
    val endOffsets = getInts(json, "endOffsets")
    val words = getStrings(json, "words")
    val tags = getLabelsOpt(json, "tags")
    val lemmas = getLabelsOpt(json, "lemmas")
    val entities = getLabelsOpt(json, "entities")
    val norms = getLabelsOpt(json, "norms")
    val chunks = getLabelsOpt(json, "chunks")
    val syntacticTree = None // TODO: Are these not serialized?
    val graphs = {
      val preferredSize = words.length
      val graphs = (json \ "graphs").extract[JObject].obj.map { case (key, json) =>
        key -> toDirectedGraph(json, Some(preferredSize))
      }.toMap

      GraphMap(graphs)
    }
    val relations = None // TODO: Are these not serialized?
    val parsedSentence = Sentence(
      raw, startOffsets, endOffsets, words,
      tags, lemmas, entities, norms, chunks, syntacticTree, graphs, relations
    )

    parsedSentence
  }

  def toDirectedGraph(json: JValue, preferredSizeOpt: Option[Int] = None): DirectedGraph[String] = {
    val edges = (json \ "edges").extract[List[Edge[String]]]
    // The roots remain for backward compatibility, but they are ignored.
    val roots = (json \ "roots").extract[Set[Int]]

    // The roots are not passed so that they are recalculated, correctly.
    new DirectedGraph(edges, preferredSizeOpt)
  }

  private def getStringOption(json: JValue, key: String): Option[String] = json \ key match {
    case JString(s) => Some(s)
    case _ => None
  }
}
