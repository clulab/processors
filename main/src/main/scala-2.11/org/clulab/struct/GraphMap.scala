package org.clulab.struct

import scala.collection.mutable

class GraphMap protected extends mutable.HashMap[String, DirectedGraph[String]] {
  override def initialSize: Int = 2 // we have very few dependency types, so let's create a small hash to save memory
}

object GraphMap {
  val UNIVERSAL_BASIC: String = GraphMapNames.UNIVERSAL_BASIC
  val UNIVERSAL_ENHANCED: String = GraphMapNames.UNIVERSAL_ENHANCED
  val STANFORD_BASIC: String = GraphMapNames.STANFORD_BASIC
  val STANFORD_COLLAPSED: String = GraphMapNames.STANFORD_COLLAPSED
  val SEMANTIC_ROLES: String = GraphMapNames.SEMANTIC_ROLES
  val ENHANCED_SEMANTIC_ROLES: String = GraphMapNames.ENHANCED_SEMANTIC_ROLES
  val HYBRID_DEPENDENCIES: String = GraphMapNames.HYBRID_DEPENDENCIES

  def apply(): GraphMap = new GraphMap()

  def apply(existing: Map[String, DirectedGraph[String]]): GraphMap = {
    val gm = GraphMap()
    gm ++= existing
  }
}
