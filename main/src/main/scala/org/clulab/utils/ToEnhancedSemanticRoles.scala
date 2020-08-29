package org.clulab.utils

import org.clulab.processors.Sentence
import org.clulab.struct.{DirectedGraph, DirectedGraphIndex}

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
    val rolesIndex = semanticRoles.toDirectedGraphIndex
    val depsIndex = basicDependencies.toDirectedGraphIndex

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
    // TODO
  }

  /**
   * Propagates arguments between conjoined verbs
   * The store buys and sells cameras => A0 from 2 to 1 and from 4 to 1; A1 from 2 to 5 and from 4 to 5
   */
  def propagateArgsInConjPredicates(sentence: Sentence,
                                    depsIndex: DirectedGraphIndex[String],
                                    rolesIndex: DirectedGraphIndex[String]): Unit = {
    // TODO
  }

  /**
   * Propagates conjoined subjects and objects to same verb (works for SD and UD)
   * Paul and Mary are reading a book => A0 from 4 to 0 and from 4 to 2
   * John is reading a book and a newspaper => A1 from 2 to 4 and from 2 to 7
   */
  def propagateConjArgs(sentence: Sentence,
                        depsIndex: DirectedGraphIndex[String],
                        rolesIndex: DirectedGraphIndex[String]): Unit = {
    // TODO
  }
}
