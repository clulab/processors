package org.clulab.serialization.json4StopGraph

import org.clulab.processors.Document
import org.clulab.serialization.json.stringify
import org.clulab.struct.DirectedGraph
import org.clulab.struct.GraphMap
import org.json4s._
import org.json4s.JsonDSL._

import java.io.PrintWriter

class JSONSerializer(printWriter: PrintWriter) {
  implicit val formats = DefaultFormats

  def serialize(doc: Document): Unit = printWriter.println(toJson(doc))

  def toJson(doc: Document): String = {
    val jValue = toJValue(doc)
    val json = stringify(jValue, pretty = true)

    json
  }

  def toJValue(directedGraph: DirectedGraph[String]): JValue = {
    directedGraph.edges.map { edge =>
      // This happens to be both the way to sort and to print.
      (edge.source, edge.destination, edge.relation)
    }
    .sorted
    .map { triple =>
      new JArray(List(triple._1, triple._2, triple._3))
    }
  }

  def toJValue(doc: Document): JValue = {
    doc.sentences.toList.map { sentence =>

      def graphToJValueOpt(key: String): Option[JValue] = sentence.graphs.get(key).map(toJValue)

      val jTokens = sentence.words.zipWithIndex.toList.map { case (word, index) =>
        new JArray(List(word, index))
      }
      val jChunksOpt = sentence.chunks.map { chunks =>
        chunks.zipWithIndex.toList.map { case (chunk, index) =>
          new JArray(List(chunk, index))
        }
      }
      val jStarts = sentence.startOffsets.toList
      val jEnds = sentence.endOffsets.toList
      val jBasicSyntacticDependenciesOpt    = graphToJValueOpt(GraphMap.UNIVERSAL_BASIC)
      val jEnhancedSyntacticDependenciesOpt = graphToJValueOpt(GraphMap.UNIVERSAL_ENHANCED)
      val jSemanticDependenciesOpt          = graphToJValueOpt(GraphMap.SEMANTIC_ROLES)
      val jEnhancedSemanticDependenciesOpt  = graphToJValueOpt(GraphMap.ENHANCED_SEMANTIC_ROLES)
      val jSentence =
          ("Tokens" -> jTokens) ~
          ("Chunks" -> jChunksOpt) ~
          ("Start_character_offsets" -> jStarts) ~
          ("End_character_offsets" -> jEnds) ~
          ("Basic_syntactic_dependencies"    -> jBasicSyntacticDependenciesOpt) ~
          ("Enhanced_syntactic_dependencies" -> jEnhancedSyntacticDependenciesOpt) ~
          ("Semantic_dependencies"           -> jSemanticDependenciesOpt) ~
          ("Enhanced_semantic_dependencies"  -> jEnhancedSemanticDependenciesOpt)

      jSentence
    }
  }
}
