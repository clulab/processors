package org.clulab.serialization

import org.clulab.odin._
import org.clulab.processors.{Document, Sentence}
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.clulab.struct.{DirectedGraph, Edge, GraphMap}
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native._


package object json {

  trait JSONSerialization {

    def jsonAST: JValue

    def json(pretty: Boolean = false): String =
      if (pretty) prettyJson(renderJValue(jsonAST))
      else compactJson(renderJValue(jsonAST))

  }

  private def argsAST(arguments: Map[String, Seq[Mention]]): JObject = {
    val args = arguments.map {
      case (name, mentions) => name -> JArray(mentions.map(_.jsonAST).toList)
    }
    JObject(args.toList)
  }

  implicit val formats = org.json4s.DefaultFormats

  implicit class MentionOps(m: Mention) extends JSONSerialization {
    def jsonAST: JValue = m match {
      case tb: TextBoundMention => TextBoundMentionOps(tb).jsonAST
      case em: EventMention => EventMentionOps(em).jsonAST
      case rm: RelationMention => RelationMentionOps(rm).jsonAST
    }

    // A mention only only contains a pointer to a document, so
    // create a Seq[Mention] whose jsonAST includes
    // an accompanying json map of docEquivHash -> doc's json
    def completeAST: JValue = Seq(m).jsonAST

    /**
      * Serialize mentions to json file
      */
    def saveJSON(file: String, pretty: Boolean): Unit = {
      require(file.endsWith(".json"), "file should have .json extension")
      Files.write(Paths.get(file), Seq(m).json(pretty).getBytes(StandardCharsets.UTF_8))
    }
    def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)
  }

  implicit class TextBoundMentionOps(tb: TextBoundMention) extends JSONSerialization {

    def jsonAST: JValue = {
      ("type" -> "TextBoundMention") ~
      ("labels" -> tb.labels) ~
      ("tokenInterval" -> Map("start" -> tb.tokenInterval.start, "end" -> tb.tokenInterval.end)) ~
      ("characterStartOffset" -> tb.startOffset) ~
      ("characterEndOffset" -> tb.endOffset) ~
      ("sentence" -> tb.sentence) ~
      ("document" -> tb.document.equivalenceHash.toString) ~
      ("keep" -> tb.keep) ~
      ("foundBy" -> tb.foundBy)
    }
  }

  implicit class EventMentionOps(em: EventMention) extends JSONSerialization {

    def jsonAST: JValue = {
      ("type" -> "EventMention") ~
      ("labels" -> em.labels) ~
      ("trigger" -> em.trigger.jsonAST) ~
      ("arguments" -> argsAST(em.arguments)) ~
      ("tokenInterval" -> Map("start" -> em.tokenInterval.start, "end" -> em.tokenInterval.end)) ~
      ("characterStartOffset" -> em.startOffset) ~
      ("characterEndOffset" -> em.endOffset) ~
      ("sentence" -> em.sentence) ~
      ("document" -> em.document.equivalenceHash.toString) ~
      ("keep" -> em.keep) ~
      ("foundBy" -> em.foundBy)
    }
  }

  implicit class RelationMentionOps(rm: RelationMention) extends JSONSerialization {
    def jsonAST: JValue = {
      ("type" -> "RelationMention") ~
      ("labels" -> rm.labels) ~
      ("arguments" -> argsAST(rm.arguments)) ~
      ("tokenInterval" -> Map("start" -> rm.tokenInterval.start, "end" -> rm.tokenInterval.end)) ~
      ("characterStartOffset" -> rm.startOffset) ~
      ("characterEndOffset" -> rm.endOffset) ~
      ("sentence" -> rm.sentence) ~
      ("document" -> rm.document.equivalenceHash.toString) ~
      ("keep" -> rm.keep) ~
      ("foundBy" -> rm.foundBy)
    }
  }

  /** For sequences of mentions */
  implicit class MentionSeq(mentions: Seq[Mention]) extends JSONSerialization {

    def jsonAST: JValue = JSONSerializer.jsonAST(mentions)

    /**
      * Serialize mentions to json file
      */
    def saveJSON(file: String, pretty: Boolean): Unit = {
      require(file.endsWith(".json"), "file should have .json extension")
      Files.write(Paths.get(file), mentions.json(pretty).getBytes(StandardCharsets.UTF_8))
    }
    def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)
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
      // field and value are removed when value is not present
      ("id" -> doc.id) ~
      ("text" -> doc.text) ~
      ("sentences" -> doc.sentences.map(_.jsonAST).toList)
      // TODO: handle discourse tree
      //("discourse-tree" -> discourseTree)
    }

    /**
      * Serialize Document to json file
      */
    def saveJSON(file: String, pretty: Boolean): Unit = {
      require(file.endsWith(".json"), "file should have .json extension")
      Files.write(Paths.get(file), doc.json(pretty).getBytes(StandardCharsets.UTF_8))
    }
    def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)
  }


  /** For Sentence */
  implicit class SentenceOps(s: Sentence) extends JSONSerialization {

    def jsonAST: JValue = {
      ("words" -> s.words.toList) ~
      ("startOffsets" -> s.startOffsets.toList) ~
      ("endOffsets" -> s.endOffsets.toList) ~
      ("tags" -> s.tags.toSerializableJSON) ~
      ("lemmas" -> s.lemmas.toSerializableJSON) ~
      ("entities" -> s.entities.toSerializableJSON) ~
      ("norms" -> s.norms.toSerializableJSON) ~
      ("chunks" -> s.chunks.toSerializableJSON) ~
      ("graphs" -> s.dependenciesByType.jsonAST)
      // TODO: handle tree
      //("syntactic-tree") -> syntacticTree)
    }

    /**
      * Serialize Sentence to json file
      */
    def saveJSON(file: String, pretty: Boolean): Unit = {
      require(file.endsWith(".json"), "file should have .json extension")
      Files.write(Paths.get(file), s.json(pretty).getBytes(StandardCharsets.UTF_8))
    }
    def saveJSON(file: File, pretty: Boolean): Unit = saveJSON(file.getAbsolutePath, pretty)
  }
}
