package org.clulab.utils

import org.clulab.processors.Sentence
import org.clulab.struct.{DirectedGraph, DirectedGraphIndex, Edge}

import scala.collection.mutable.ListBuffer

/**
 * Converts CoNLL-style semantic role dependencies into an enahnced format, inspired by enhanced universal dependencies
 *
 * @author Mihai
 * Date 08/28/2020
 */
object ToEnhancedSemanticRoles {
  def generateEnhancedSemanticRoles(sentence:Sentence,
                                    basicDependencies:DirectedGraph[String],
                                    semanticRoles:DirectedGraph[String]): DirectedGraph[String] = {
    val rolesIndex = semanticRoles.toDirectedGraphIndex(basicDependencies.size)
    val depsIndex = basicDependencies.toDirectedGraphIndex(basicDependencies.size)

    collapsePrepositions(sentence, depsIndex, rolesIndex)
    propagateArgsInConjPredicates(sentence, depsIndex, rolesIndex)
    propagateConjArgs(sentence, depsIndex, rolesIndex)

    rolesIndex.toDirectedGraph
  }

  /**
   * Appends prepositions to arg names that end on a preposition, and moves the modifier pointer to the prep's object
   * Mary gave a book to Jane => Ax_to from 1 to 5
   */
  def collapsePrepositions(sentence: Sentence,
                           depsIndex: DirectedGraphIndex[String],
                           rolesIndex: DirectedGraphIndex[String]): Unit = {
    val toRemove = new ListBuffer[Edge[String]]

    for(predicate <- rolesIndex.outgoingEdges.indices) {
      for(arg <- rolesIndex.outgoingEdges(predicate)) {
        val modifier = arg._1
        val label = arg._2

        val prepObjects = depsIndex.findByModifierAndPattern(modifier, "case|mark".r)
        if(prepObjects.nonEmpty) { // by definition we can have at most 1 dependency here
          val prepObject = prepObjects.head
          val newArg = prepObject.source
          val newLabel =
            if(label == "Ax") label + "_" + sentence.words(prepObject.destination).toLowerCase()
            else label
          rolesIndex.addEdge(predicate, newArg, newLabel)
          toRemove += Edge(predicate, modifier, label)
        }
      }
    }

    toRemove.foreach(e => rolesIndex.removeEdge(e.source, e.destination, e.relation))
  }

  /**
   * Propagates arguments between conjoined verbs
   * The store buys and sells cameras => A0 from 2 to 1 and from 4 to 1; A1 from 2 to 5 and from 4 to 5
   */
  def propagateArgsInConjPredicates(sentence: Sentence,
                                    depsIndex: DirectedGraphIndex[String],
                                    rolesIndex: DirectedGraphIndex[String]): Unit = {
    val conjs = depsIndex.findByName("conj").sortBy(_.source)
    val toAdd = new ListBuffer[Edge[String]]

    for(conj <- conjs) {
      val left = math.min(conj.source, conj.destination)
      val right = math.max(conj.source, conj.destination)

      val leftRoles = rolesIndex.findByHeadAndPattern(left, "^A*".r).toSet
      val rightRoles = rolesIndex.findByHeadAndPattern(right, "^A*".r).toSet

      // add left roles to right predicate, if not present already
      for(leftRole <- leftRoles) {
        val modifier = leftRole.destination
        val label = leftRole.relation
        if(! contains(rightRoles, right, label)) {
          val e = Edge(right, modifier, label)
          toAdd += e
        }
      }

      // add right roles to left predicate, if not present already
      for(rightRole <- rightRoles) {
        val modifier = rightRole.destination
        val label = rightRole.relation
        if(! contains(leftRoles, left, label)) {
          val e = Edge(left, modifier, label)
          toAdd += e
        }
      }
    }

    for(e <- toAdd) rolesIndex.addEdge(e.source, e.destination, e.relation)
  }

  private def contains(roles: Set[Edge[String]], head: Int, label: String): Boolean = {
    for(role <- roles) {
      if(role.source == head && role.relation == label) {
        return true
      }
    }
    false
  }

  /**
   * Propagates conjoined subjects and objects to same verb
   * Paul and Mary are reading a book => A0 from 4 to 0 and from 4 to 2
   * John is reading a book and a newspaper => A1 from 2 to 4 and from 2 to 7
   */
  def propagateConjArgs(sentence: Sentence,
                        depsIndex: DirectedGraphIndex[String],
                        rolesIndex: DirectedGraphIndex[String]): Unit = {
    val conjs = depsIndex.findByName("conj").sortBy(_.source)
    val toAdd = new ListBuffer[Edge[String]]

    for(conj <- conjs) {
      val left = math.min(conj.source, conj.destination)
      val right = math.max(conj.source, conj.destination)

      val leftRoles = rolesIndex.findByModifierAndPattern(left, "^A*".r).toSet
      val rightRoles = rolesIndex.findByModifierAndPattern(right, "^A*".r).toSet

      // add left roles to right argument, if not present already
      for(leftRole <- leftRoles) {
        val predicate = leftRole.source
        val label = leftRole.relation
        val e = Edge(predicate, right, label)
        if(! rightRoles.contains(e)) {
          toAdd += e
        }
      }

      // add right roles to left argument, if not present already
      for(rightRole <- rightRoles) {
        val predicate = rightRole.source
        val label = rightRole.relation
        val e = Edge(predicate, left, label)
        if(! leftRoles.contains(e)) {
          toAdd += e
        }
      }
    }

    for(e <- toAdd) rolesIndex.addEdge(e.source, e.destination, e.relation)
  }
}
