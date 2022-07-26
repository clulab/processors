package org.clulab.struct

import scala.collection.mutable

object DependencyMap {
  def apply(): DepdendencyMap = new mutable.HashMap[Int, DirectedGraph[String]](2,mutable.HashMap.defaultLoadFactor)

  val STANFORD_BASIC = 0 // basic Stanford dependencies
  val STANFORD_COLLAPSED = 1 // collapsed Stanford dependencies
  val SEMANTIC_ROLES = 2 // semantic roles from CoNLL 2008-09, which includes PropBank and NomBank
}