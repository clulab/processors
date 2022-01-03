package org.clulab.struct

import scala.collection.mutable

object GraphMap {
  // This was previously a class inheriting from HashMap.  However,
  // [warn] ...: inheritance from class HashMap in package mutable is deprecated (since 2.13.0): HashMap will be made final; use .withDefault for the common use case of computing a default value
  type GraphMap = mutable.HashMap[String, DirectedGraph[String]]

  val UNIVERSAL_BASIC = "universal-basic" // basic Universal dependencies
  val UNIVERSAL_ENHANCED = "universal-enhanced" // collapsed (or enhanced) Universal dependencies
  val STANFORD_BASIC = "stanford-basic" // basic Stanford dependencies
  val STANFORD_COLLAPSED = "stanford-collapsed" // collapsed Stanford dependencies
  val SEMANTIC_ROLES = "semantic-roles" // semantic roles from CoNLL 2008-09, which includes PropBank and NomBank
  val ENHANCED_SEMANTIC_ROLES = "enhanced-semantic-roles" // enhanced semantic roles
  val HYBRID_DEPENDENCIES = "hybrid" // graph that merges ENHANCED_SEMANTIC_ROLES and UNIVERSAL_ENHANCED

  def apply(): GraphMap = {
    // we have very few dependency types, so let's create a small hash to save memory.
    new GraphMap(2, mutable.HashMap.defaultLoadFactor)
  }

  def apply(existing: scala.collection.Map[String, DirectedGraph[String]]): GraphMap = {
    val gm = GraphMap()
    gm ++= existing
  }
}
