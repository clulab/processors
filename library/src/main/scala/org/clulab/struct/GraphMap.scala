package org.clulab.struct

import scala.collection.mutable

object GraphMap {
  type ImmutableType = Map[String, DirectedGraph[String]]
  type MutableType = mutable.Map[String, DirectedGraph[String]]

  val immutableEmpty: ImmutableType = Map.empty
  val mutableEmpty: MutableType = mutable.Map.empty[String, DirectedGraph[String]]

  val UNIVERSAL_BASIC = "universal-basic" // basic Universal dependencies
  val UNIVERSAL_ENHANCED = "universal-enhanced" // collapsed (or enhanced) Universal dependencies
  val STANFORD_BASIC = "stanford-basic" // basic Stanford dependencies
  val STANFORD_COLLAPSED = "stanford-collapsed" // collapsed Stanford dependencies
  val SEMANTIC_ROLES = "semantic-roles" // semantic roles from CoNLL 2008-09, which includes PropBank and NomBank
  val ENHANCED_SEMANTIC_ROLES = "enhanced-semantic-roles" // enhanced semantic roles
  val HYBRID_DEPENDENCIES = "hybrid" // graph that merges ENHANCED_SEMANTIC_ROLES and UNIVERSAL_ENHANCED
}
