package org.clulab.struct

import scala.collection.mutable

class GraphMap protected extends mutable.HashMap[String, DirectedGraph[String]] {
  override def initialSize: Int = 2 // we have very few dependency types, so let's create a small hash to save memory
}

object GraphMap extends GraphMapNames {

  def apply(): GraphMap = new GraphMap()

  def apply(existing: Map[String, DirectedGraph[String]]): GraphMap = {
    val gm = GraphMap()
    gm ++= existing
  }
}
