package org.clulab.struct

import scala.collection.mutable

object GraphMap extends GraphMapNames {

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
