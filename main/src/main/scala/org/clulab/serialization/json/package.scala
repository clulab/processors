package org.clulab.serialization

import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.{DirectedGraph, Edge, GraphMap}
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson._


package object json {
  val DOCUMENT_ATTACHMENTS_KEY = "documentAttachments"
  val DOCUMENT_ATTACHMENTS_BUILDER_KEY = "builder"
  val DOCUMENT_ATTACHMENTS_VALUE_KEY = "value"

  implicit val formats = DefaultFormats

  /** Method for debugging json */
  def stringify(json: JValue, pretty: Boolean): String = pretty match {
    case true => prettyJson(renderJValue(json))
    case false => compactJson(renderJValue(json))
  }

  // Arrays cannot be directly converted to JValue
  implicit class ArrayOps(s: Option[Array[String]]) {
    def toSerializableJSON: Option[List[String]] = s match {
      case Some(s) => Some(s.toList)
      case None => None
    }
  }

  implicit class ODirectedGraphOps(odg: Option[DirectedGraph[String]]) {
    def toSerializableJSON: Option[JValue] = odg match {
      case Some(g) => Some(g.jsonAST)
      case None => None
    }
  }

  implicit class DirectedGraphOps(dg: DirectedGraph[String]) extends JSONSerialization {
    def jsonAST: JValue = {
      ("edges" -> dg.edges.map(_.jsonAST)) ~
      // The roots are being saved for backward compatibility and human consumption.
      ("roots" -> dg.roots)
    }
  }

  implicit class EdgeOps(edge: Edge[String]) extends JSONSerialization {
    def jsonAST: JValue = {
      ("source" -> edge.source) ~
      ("destination" -> edge.destination) ~
      ("relation" -> edge.relation.toString)
    }
  }

  implicit class GraphMapOps(gm: GraphMap) extends JSONSerialization {
    def jsonAST: JValue = Extraction.decompose(gm.toMap.mapValues(_.jsonAST))
  }

  /** For Document */
  implicit class DocOps(doc: Document) extends JSONSerialization {

    def jsonAST: JValue = {
      // See also DocumentSerializer for a similar text implementation.
      val attachmentKeys = doc.getAttachmentKeys.toList.sorted
      val documentAttachments: JValue = if (attachmentKeys.nonEmpty) {
        val jFields = attachmentKeys.map { key =>
          val value = doc.getAttachment(key).get
          JField(key,
              (DOCUMENT_ATTACHMENTS_BUILDER_KEY -> JString(value.documentAttachmentBuilderFromJsonClassName)) ~
              (DOCUMENT_ATTACHMENTS_VALUE_KEY -> value.toJsonSerializer)
          )
        }
        JObject(jFields)
      }
      else JNothing

      // field and value are removed when value is not present
      val ast1 =
          ("id" -> doc.id) ~
          ("text" -> doc.text) ~
          ("sentences" -> doc.sentences.map(_.jsonAST).toList)
          // TODO: handle discourse tree
          //("discourse-tree" -> discourseTree)
      val ast2 = if (documentAttachments == JNothing) ast1
          else ast1 ~ (DOCUMENT_ATTACHMENTS_KEY -> documentAttachments)

      ast2
    }
  }


  /** For Sentence */
  implicit class SentenceOps(s: Sentence) extends JSONSerialization {

    def jsonAST: JValue = {
      ("words" -> s.words.toList) ~
      ("startOffsets" -> s.startOffsets.toList) ~
      ("endOffsets" -> s.endOffsets.toList) ~
      ("raw" -> s.raw.toList) ~
      ("tags" -> s.tags.toSerializableJSON) ~
      ("lemmas" -> s.lemmas.toSerializableJSON) ~
      ("entities" -> s.entities.toSerializableJSON) ~
      ("norms" -> s.norms.toSerializableJSON) ~
      ("chunks" -> s.chunks.toSerializableJSON) ~
      ("graphs" -> s.graphs.jsonAST)
      // TODO: handle tree
      //("syntactic-tree") -> syntacticTree)
    }

  }

}
