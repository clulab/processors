package org.clulab.processors.clu.syntax

import org.clulab.processors.Sentence
import org.clulab.struct.{DirectedGraph, DirectedGraphIndex}

/**
  * Converts Stanford basic dependencies to collapsed ones
  * This follows the rules from http://universaldependencies.org/u/overview/enhanced-syntax.html
  *   (but applied to Stanford deps rather than universal ones)
  * User: mihais
  * Date: 8/1/17
  */
object EnhancedDependencies {
  def generateEnhancedDependencies(sentence:Sentence, dg:DirectedGraph[String]): DirectedGraph[String] = {
    val dgi = dg.toDirectedGraphIndex
    collapsePrepositions(sentence, dgi)
    raiseSubjects(dgi)
    dgi.toDirectedGraph
  }

  def collapsePrepositions(sentence:Sentence, dgi:DirectedGraphIndex[String]): Unit = {
      
  }

  def raiseSubjects(dgi:DirectedGraphIndex[String]) {
    val subjects = dgi.findByName("nsubj")
    for(se <- subjects) {
      for(xcomp <- dgi.findByName("xcomp").filter(_.source == se.source)) {
        dgi.addEdge(xcomp.destination, se.destination, "nsubj")
      }
    }
  }
}
