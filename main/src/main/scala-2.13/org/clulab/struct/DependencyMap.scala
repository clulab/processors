package org.clulab.struct

import scala.collection.mutable

object DependencyMap extends DependencyMapNames {
  // This was previously a class inheriting from HashMap.  However,
  // [warn] ...: inheritance from class HashMap in package mutable is deprecated (since 2.13.0): HashMap will be made final; use .withDefault for the common use case of computing a default value.
  type DependencyMap = mutable.HashMap[String, DirectedGraph[String]]

  def apply(): DependencyMap = {
    // we have very few dependency types, so let's create a small hash to save memory.
    new DependencyMap(2, mutable.HashMap.defaultLoadFactor)
  }
}
