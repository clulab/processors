package org.clulab.struct

import scala.collection.mutable

class DependencyMap protected extends mutable.HashMap[Int, DirectedGraph[String]] {
  override def initialSize: Int = 2 // we have very few dependency types, so let's create a small hash to save memory
}

object DependencyMap {
  val STANFORD_BASIC: Int = DependencyMapNames.STANFORD_BASIC
  val STANFORD_COLLAPSED: Int = DependencyMapNames.STANFORD_COLLAPSED
  val SEMANTIC_ROLES: Int = DependencyMapNames.SEMANTIC_ROLES

  def apply(): DependencyMap = new DependencyMap()
}
