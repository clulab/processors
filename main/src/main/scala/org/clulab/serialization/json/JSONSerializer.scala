package org.clulab.serialization.json

import java.io.File
import org.clulab.processors.DocumentAttachmentBuilderFromJson
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Edge
import org.clulab.struct.{DirectedGraph, GraphMap}
import org.clulab.utils.FileUtils
import org.json4s
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.prettyJson


/** JSON serialization utilities */
// This annotation is to avoid "Compiler synthesis of Manifest and OptManifest is deprecated".
@annotation.nowarn("cat=deprecation")
object JSONSerializer {
  implicit val formats: DefaultFormats.type = DefaultFormats

  def jsonAST(s: String): JValue = parse(s)

  def jsonAST(f: File): JValue = jsonAST(FileUtils.getTextFromFile(f))

  protected def addDocumentAttachments(doc: Document, jValue: JValue): Unit = {
    // See also DocumentSerializer for text version of nearly the same thing.
    (jValue \ DOCUMENT_ATTACHMENTS_KEY) match {
      case jObject: JObject =>
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
              doc.addAttachment(key, documentAttachment)
            case jValue: JValue =>
              val text = prettyJson(jValue)
              throw new RuntimeException(s"ERROR: While deserializing document attachments expected JObject but found this: $text")
            // case _ => // noop.  It should never get here.  (Famous last words.)
            case null => // noop.  It should never get here.  (Famous last words.)  Scala 3 prefers null over _.
          }
        }
      case _ => // Leave documentAttachments as is: None
    }
  }

  def toDocument(json: JValue): Document = {
    // recover sentences
    val sentences = (json \ "sentences").asInstanceOf[JArray].arr.map(sjson => toSentence(sjson)).toArray
    // initialize document
    val d = Document(sentences)
    // update id
    d.id = getStringOption(json, "id")
    // update text
    d.text = getStringOption(json, "text")
    addDocumentAttachments(d, json)
    d
  }
  def toDocument(docHash: String, djson: JValue): Document = toDocument(djson \ docHash)
  def toDocument(f: File): Document = toDocument(jsonAST(f))
  def toDocument(s: String): Document = toDocument(jsonAST(s))

  def toSentence(json: JValue): Sentence = {

    def getLabels(json: JValue, k: String): Option[Array[String]] = json \ k match {
      case JNothing => None
      case contents => Some(contents.extract[Array[String]])
    }

    val s = json.extract[Sentence]
    val preferredSize = s.words.length
    // build dependencies
    val graphs = (json \ "graphs").extract[JObject].obj.map { case (key, json) =>
      key -> toDirectedGraph(json, Some(preferredSize))
    }.toMap
    s.graphs = GraphMap(graphs)
    // build labels
    s.tags = getLabels(json, "tags")
    s.lemmas = getLabels(json, "lemmas")
    s.entities = getLabels(json, "entities")
    s.norms = getLabels(json, "norms")
    s.chunks = getLabels(json, "chunks")
    s
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
