package org.clulab.struct

import scala.collection.mutable


class DependencyMap extends mutable.HashMap[Int, DirectedGraph[String]] {
  override def initialSize:Int = 2 // we have very few dependency types, so let's create a small hash to save memory
}

object DependencyMap {
  val STANFORD_BASIC = 0 // basic Stanford dependencies
  val STANFORD_COLLAPSED = 1 // collapsed Stanford dependencies
  val SEMANTIC_ROLES = 2 // semantic roles from CoNLL 2008-09, which includes PropBank and NomBank
}