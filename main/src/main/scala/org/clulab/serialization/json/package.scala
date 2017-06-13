package org.clulab.serialization

import org.clulab.processors.{Document, Sentence}
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.clulab.struct.{DirectedGraph, Edge, GraphMap}
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson._


package object json {

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
