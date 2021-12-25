package org.clulab.struct

import scala.collection.mutable

/**
  * Stores the edges of a graph in a hash map for quick access
  * The edges are indexed by the tuple (source, destination)
  */
class EdgeMap[E] (private val edgeMap: mutable.HashMap[(Int, Int), E] = new mutable.HashMap[(Int, Int), E]) {
  def add(source: Int, destination: Int, value: E): Unit = {
    edgeMap += (source, destination) -> value
  }

  def += (source: Int, destination: Int, value: E): Unit = {
    add(source, destination, value)
  }

  def apply(source: Int, destination: Int): E = {
    edgeMap((source, destination))
  }
}