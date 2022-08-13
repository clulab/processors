package org.clulab.struct

import scala.collection.mutable

object GraphMap {
  val UNIVERSAL_BASIC: String = GraphMapNames.UNIVERSAL_BASIC
  val UNIVERSAL_ENHANCED: String = GraphMapNames.UNIVERSAL_ENHANCED
  val STANFORD_BASIC: String = GraphMapNames.STANFORD_BASIC
  val STANFORD_COLLAPSED: String = GraphMapNames.STANFORD_COLLAPSED
  val SEMANTIC_ROLES: String = GraphMapNames.SEMANTIC_ROLES
  val ENHANCED_SEMANTIC_ROLES: String = GraphMapNames.ENHANCED_SEMANTIC_ROLES
  val HYBRID_DEPENDENCIES: String = GraphMapNames.HYBRID_DEPENDENCIES

  // This was previously a class inheriting from HashMap.  However,
  // [warn] ...: inheritance from class HashMap in package mutable is deprecated (since 2.13.0): HashMap will be made final; use .withDefault for the common use case of computing a default value
  type GraphMap = mutable.HashMap[String, DirectedGraph[String]]

  def apply(): GraphMap = {
    // we have very few dependency types, so let's create a small hash to save memory.
    new GraphMap(2, mutable.HashMap.defaultLoadFactor)
  }

  def apply(existing: scala.collection.Map[String, DirectedGraph[String]]): GraphMap = {
    val gm = GraphMap()
    gm ++= existing
  }
}
