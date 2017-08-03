package org.clulab.processors.clu.syntax

import org.clulab.processors.Sentence
import org.clulab.struct.{DirectedGraph, DirectedGraphIndex, Edge}

import scala.collection.mutable.ListBuffer

/**
  * Converts Stanford basic dependencies to collapsed ones
  * This follows the rules from http://universaldependencies.org/u/overview/enhanced-syntax.html
  *   (but applied to Stanford deps rather than universal ones)
  * We support:
  * - Collapsing of prepositions to the prep_* label
  * - Controlled/raised subjects
  * - Propagate subjects and objects in conjoined verbs
  * - Propagate conjoined subjects and objects to same verb
  * - Push subjects/objects inside relative clauses
  * User: mihais
  * Date: 8/1/17
  */
object EnhancedDependencies {
  def generateEnhancedDependencies(sentence:Sentence, dg:DirectedGraph[String]): DirectedGraph[String] = {
    val dgi = dg.toDirectedGraphIndex
    collapsePrepositions(sentence, dgi)
    raiseSubjects(dgi)
    propagateSubjectsAndObjectsInConjVerbs(dgi)
    propagateConjSubjectsAndObjects(dgi)
    pushSubjectsObjectsInsideRelativeClauses(dgi)
    dgi.toDirectedGraph
  }

  /**
    * Collapses prep + pobj into prep_x
    * Mary gave a book to Jane => prep_to from 1 to 5
    * @param sentence
    * @param dgi
    */
  def collapsePrepositions(sentence:Sentence, dgi:DirectedGraphIndex[String]) {
    val toRemove = new ListBuffer[Edge[String]]
    val preps = dgi.findByName("prep")
    for(prep <- preps) {
      toRemove += prep
      val word = sentence.words(prep.destination)
      for(pobj <- dgi.findByName("pobj").filter(_.source == prep.destination)) {
        dgi.addEdge(prep.source, pobj.destination, s"prep_$word")
        toRemove += pobj
      }
    }
    toRemove.foreach(e => dgi.removeEdge(e.source, e.destination, e.relation))
  }

  /**
    * Pushes subjects inside xcomp clauses
    * Mary wants to buy a book => nsubj from 3 to 0
    * @param dgi
    */
  def raiseSubjects(dgi:DirectedGraphIndex[String]) {
    val subjects = dgi.findByName("nsubj")
    for(se <- subjects) {
      for(xcomp <- dgi.findByName("xcomp").filter(_.source == se.source)) {
        dgi.addEdge(xcomp.destination, se.destination, "nsubj")
      }
    }
  }

  /**
    * Propagates subjects/objects between conjoined verbs
    * The store buys and sells cameras => nsubj from 2 to 1 and from 4 to 1; dobj from 2 to 5 and from 4 to 5
    * @param dgi
    */
  def propagateSubjectsAndObjectsInConjVerbs(dgi:DirectedGraphIndex[String]) {
    // TODO
  }

  def propagateConjSubjectsAndObjects(dgi:DirectedGraphIndex[String]) {
    // TODO
  }
  
  def pushSubjectsObjectsInsideRelativeClauses(dgi:DirectedGraphIndex[String]) {
    // TODO
  }
}
