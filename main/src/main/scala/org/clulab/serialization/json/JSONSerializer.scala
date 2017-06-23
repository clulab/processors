package org.clulab.serialization.json

import java.io.File
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.{DirectedGraph, GraphMap}
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._


/** JSON serialization utilities */
object JSONSerializer {

  implicit val formats = DefaultFormats

  def jsonAST(f: File): JValue = {
    val source = scala.io.Source.fromFile(f)
    val contents = source.getLines.mkString
    source.close()
    parse(contents)
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
    d
  }
  def toDocument(docHash: String, djson: JValue): Document = toDocument(djson \ docHash)
  def toDocument(f: File): Document = toDocument(jsonAST(f))

  def toSentence(json: JValue): Sentence = {

    def getLabels(json: JValue, k: String): Option[Array[String]] = json \ k match {
      case JNothing => None
      case contents => Some(contents.extract[Array[String]])
    }

    val s = json.extract[Sentence]
    // build dependencies
    val graphs = (json \ "graphs").extract[Map[String, DirectedGraph[String]]]
    s.graphs = GraphMap(graphs)
    // build labels
    s.tags = getLabels(json, "tags")
    s.lemmas = getLabels(json, "lemmas")
    s.entities = getLabels(json, "entities")
    s.norms = getLabels(json, "norms")
    s.chunks = getLabels(json, "chunks")
    s
  }

  private def getStringOption(json: JValue, key: String): Option[String] = json \ key match {
    case JString(s) => Some(s)
    case _ => None
  }
}
