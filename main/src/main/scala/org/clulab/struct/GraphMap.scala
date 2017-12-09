package org.clulab.struct

import scala.collection.mutable


class GraphMap extends mutable.HashMap[String, DirectedGraph[String]] {
  override def initialSize:Int = 2 // we have very few dependency types, so let's create a small hash to save memory
}

object GraphMap {
  val UNIVERSAL_BASIC = "universal-basic" // basic Universal dependencies
  val UNIVERSAL_ENHANCED = "universal-enhanced" // collapsed (or enhanced) Universal dependencies
  val STANFORD_BASIC = "stanford-basic" // basic Stanford dependencies
  val STANFORD_COLLAPSED = "stanford-collapsed" // collapsed Stanford dependencies
  val SEMANTIC_ROLES = "semantic-roles" // semantic roles from CoNLL 2008-09, which includes PropBank and NomBank

  def apply(existing: Map[String, DirectedGraph[String]]): GraphMap = {
    val gm = new GraphMap
    gm ++= existing
  }
}