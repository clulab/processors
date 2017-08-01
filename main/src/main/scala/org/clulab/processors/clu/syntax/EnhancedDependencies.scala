package org.clulab.processors.clu.syntax

import org.clulab.struct.DirectedGraph

/**
  * Converts Stanford basic dependencies to collapsed ones
  * This follows the rules from http://universaldependencies.org/u/overview/enhanced-syntax.html
  *   (but applied to Stanford deps rather than universal ones)
  * User: mihais
  * Date: 8/1/17
  */
object EnhancedDependencies {
  def generateEnhancedDependencies(dg:DirectedGraph[String]): DirectedGraph[String] = {
    // TODO
    dg
  }
}
