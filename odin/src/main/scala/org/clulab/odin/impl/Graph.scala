package org.clulab.odin.impl

import org.clulab.processors.Document
import org.clulab.struct.{DirectedGraph, GraphMap}

trait Graph {
  def retrieveGraph(sent: Int, doc: Document, graphType: String = GraphMap.STANFORD_COLLAPSED): Option[DirectedGraph[String]] =
    doc.sentences(sent).graphs.get(graphType) match {
      case Some(graph) => Some(graph)
      case None => None
      //throw new OdinException(s"sentence does not have graph with name '$graphType'")
    }

  def incomingEdges(sent: Int, doc: Document): Array[Array[(Int, String)]] = retrieveGraph(sent, doc) match {
    case Some(graph) => graph.incomingEdges
    case None => Array.empty
  }

  def outgoingEdges(sent: Int, doc: Document): Array[Array[(Int, String)]] = retrieveGraph(sent, doc) match {
    case Some(graph) => graph.outgoingEdges
    case None => Array.empty
  }

  def incoming(tok: Int, sent: Int, doc: Document): Array[String] = {
    val edges = incomingEdges(sent, doc)
    if (edges isDefinedAt tok) edges(tok).map(_._2) else Array.empty
  }

  def outgoing(tok: Int, sent: Int, doc: Document): Array[String] = {
    val edges = outgoingEdges(sent, doc)
    if (edges isDefinedAt tok) edges(tok).map(_._2) else Array.empty
  }
}
