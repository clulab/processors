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

  def apply(sourceDest: (Int, Int)): E = {
    edgeMap(sourceDest)
  }

  def contains(sourceDest: (Int, Int)): Boolean = {
    edgeMap.contains(sourceDest)
  }

  def keySet:scala.collection.Set[(Int, Int)] = edgeMap.keySet
  def keys: Iterable[(Int, Int)] = edgeMap.keys
}