package org.clulab.processors.webapp2.serialization

import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.DirectedGraph
import org.json4s.JsonAST.JArray
import org.json4s.{JObject, JValue}
import org.json4s.JsonDSL._

class SyntaxObj(val doc: Document, val text: String) {

  def mkJson: JObject = {
    ("text" -> text) ~
    ("entities" -> mkJsonFromTokens) ~
    ("relations" -> mkJsonFromDependencies)
  }

  def mkJsonFromTokens: JValue = {
    var offset = 0

    val tokens = doc.sentences.flatMap { sent =>
      val tokens = sent.words.indices.map(i => mkJsonFromToken(sent, offset, i))
      offset += sent.words.length
      tokens
    }.toList
    JArray(tokens)
  }

  def mkJsonFromToken(sent: Sentence, offset: Int, i: Int): JValue = {
    JArray(List(
      s"T${offset + i + 1}", // token id (starts at one, not zero)
      sent.tags.get(i), // lets assume that tags are always available
      JArray(List(JArray(List(sent.startOffsets(i), sent.endOffsets(i)))))
    ))
  }

  def mkJsonFromDependencies: JValue = {
    var offset = 1

    val rels = doc.sentences.flatMap { sent =>
      var relId = 0
      val deps = sent.dependencies.getOrElse(new DirectedGraph[String](List.empty)) // Let's not assume that dependencies are always available
      val rels = for {
        governor <- deps.outgoingEdges.indices
        (dependent, label) <- deps.outgoingEdges(governor)
      } yield {
        val json = mkJsonFromDependency(offset + relId, offset + governor, offset + dependent, label)
        relId += 1
        json
      }
      offset += sent.words.length
      rels
    }.toList
    JArray(rels)
  }

  def mkJsonFromDependency(relId: Int, governor: Int, dependent: Int, label: String): JValue = {
    JArray(List(
      s"R$relId",
      label,
      JArray(List(
        JArray(List("governor", s"T$governor")),
        JArray(List("dependent", s"T$dependent"))
      ))
    ))
  }
}
