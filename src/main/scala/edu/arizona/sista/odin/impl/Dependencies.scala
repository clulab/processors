package edu.arizona.sista.odin.impl

import edu.arizona.sista.processors.Document
import edu.arizona.sista.struct.DirectedGraph

trait Dependencies {
  def dependencies(sent: Int, doc: Document): DirectedGraph[String] =
    doc.sentences(sent).dependencies match {
      case None => sys.error("sentence has no dependencies")
      case Some(deps) => deps
    }

  def incomingEdges(sent: Int, doc: Document): Array[Array[(Int, String)]] =
    dependencies(sent, doc).incomingEdges

  def outgoingEdges(sent: Int, doc: Document): Array[Array[(Int, String)]] =
    dependencies(sent, doc).outgoingEdges

  def incoming(tok: Int, sent: Int, doc: Document): Seq[String] = {
    val edges = incomingEdges(sent, doc)
    if (edges isDefinedAt tok) edges(tok).map(_._2) else Nil
  }

  def outgoing(tok: Int, sent: Int, doc: Document): Seq[String] = {
    val edges = outgoingEdges(sent, doc)
    if (edges isDefinedAt tok) edges(tok).map(_._2) else Nil
  }
}
