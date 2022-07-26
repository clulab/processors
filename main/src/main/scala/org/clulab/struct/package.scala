package org.clulab

import scala.collection.mutable

package object struct {
  type DepdendencyMap = mutable.HashMap[Int, DirectedGraph[String]]

  type GraphMap = mutable.HashMap[String, DirectedGraph[String]]
}
