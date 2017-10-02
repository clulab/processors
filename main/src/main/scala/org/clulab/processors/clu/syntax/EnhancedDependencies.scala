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
  * - Propagating subjects and objects in conjoined verbs
  * - Propagating conjoined subjects and objects to same verb
  * - Pushing subjects/objects inside relative clauses
  * User: mihais
  * Date: 8/1/17
  */
object EnhancedDependencies {
  def generateEnhancedDependencies(sentence:Sentence, dg:DirectedGraph[String]): DirectedGraph[String] = {
    val dgi = dg.toDirectedGraphIndex
    collapsePrepositions(sentence, dgi)
    raiseSubjects(dgi)
    pushSubjectsObjectsInsideRelativeClauses(sentence, dgi)
    propagateSubjectsAndObjectsInConjVerbs(sentence, dgi)
    propagateConjSubjectsAndObjects(sentence, dgi)
    dgi.toDirectedGraph
  }

  /**
    * Collapses prep + pobj into prep_x
    * Mary gave a book to Jane => prep_to from 1 to 5
    * @param sentence The sentence to operate on
    * @param dgi The directed graph of collapsed dependencies at this stage
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
    * @param dgi The directed graph of collapsed dependencies at this stage
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
    * @param sentence The sentence to operate on
    * @param dgi The directed graph of collapsed dependencies at this stage
    */
  def propagateSubjectsAndObjectsInConjVerbs(sentence:Sentence, dgi:DirectedGraphIndex[String]) {
    val conjs = dgi.findByName("conj").sortBy(_.source)
    val tags = sentence.tags.get
    for(conj <- conjs) {
      val left = math.min(conj.source, conj.destination)
      val right = math.max(conj.source, conj.destination)
      if(tags(left).startsWith("VB") && tags(right).startsWith("VB")) { // two verbs

        // add the subject of the left verb to the right, if the right doesn't have a subject already
        for(label <- List("nsubj", "nsubjpass", "agent")) {
          val leftSubjs = dgi.findByHeadAndName(left, label)
          val rightSubjs = dgi.findByHeadAndName(right, label)
          if (leftSubjs.nonEmpty && rightSubjs.isEmpty) {
            for (s <- leftSubjs) {
              dgi.addEdge(right, s.destination, label)
            }
          }
        }

        // add the dobj of the right verb to the left, if the left doesn't have a dobj already
        for(label <- List("dobj")) {
          val leftObjs = dgi.findByHeadAndName(left, label)
          val rightObjs = dgi.findByHeadAndName(right, label)
          if (leftObjs.isEmpty && rightObjs.nonEmpty) {
            for (o <- rightObjs) {
              dgi.addEdge(left, o.destination, label)
            }
          }
        }

        // add the prep_x of the right verb to the left, if the left doesn't have a similar prep_x already
        val rightPreps = dgi.findByHeadAndPattern(right, "^prep_*".r)
        for(rightPrep <- rightPreps) {
          val leftPreps = dgi.findByHeadAndName(left, rightPrep.relation)
          if(leftPreps.isEmpty) {
            dgi.addEdge(left, rightPrep.destination, rightPrep.relation)
          }
        }
      }
    }
  }

  /**
    * Propagates conjoined subjects and objects to same verb
    * Paul and Mary are reading a book => nsubj from 4 to 0 and from 4 to 2
    * John is reading a book and a newspaper => dobj from 2 to 4 and from 2 to 7
    * @param sentence The sentence to operate on
    * @param dgi The directed graph of collapsed dependencies at this stage
    */
  def propagateConjSubjectsAndObjects(sentence:Sentence, dgi:DirectedGraphIndex[String]) {
    val conjs = dgi.findByName("conj").sortBy(_.source)
    val tags = sentence.tags.get
    for(conj <- conjs) {
      val left = math.min(conj.source, conj.destination)
      val right = math.max(conj.source, conj.destination)

      if((tags(left).startsWith("NN") || tags(left).startsWith("PR")) &&
         (tags(right).startsWith("NN") || tags(right).startsWith("NN"))) {

        for(label <- List("nsubj", "nsubjpass", "agent", "dobj")) {
          val leftDeps = dgi.findByModifierAndName(left, label)
          val rightDeps = dgi.findByModifierAndName(right, label)

          if(leftDeps.nonEmpty && rightDeps.isEmpty) {
            for(s <- leftDeps) {
              dgi.addEdge(s.source, right, label)
            }
          }
          if(rightDeps.nonEmpty && leftDeps.isEmpty) {
            for(s <- rightDeps) {
              dgi.addEdge(s.source, left, label)
            }
          }
        }

      }
    }
  }

  /**
    * Propagates subjects and objects inside relative clauses
    * The boy who lived => nsubj from 3 to 1
    * the book, which I read, was great. => dobj from 5 to 1
    * @param sentence The sentence to operate on
    * @param dgi The directed graph of collapsed dependencies at this stage
    */
  def pushSubjectsObjectsInsideRelativeClauses(sentence:Sentence, dgi:DirectedGraphIndex[String]) {
    val rels = dgi.findByName("rcmod")
    val tags = sentence.tags.get

    for(rel <- rels) {
      val head = rel.source
      val relVerb = rel.destination
      if((tags(head).startsWith("NN") || tags(head).startsWith("PR")) &&
         tags(relVerb).startsWith("VB")) {
        var done = false
        for(label <- List("nsubj", "nsubjpass", "dobj", "agent") if ! done) {
          val deps = dgi.findByHeadAndName(relVerb, label)
          for(dep <- deps if ! done) {
            if((tags(dep.destination).startsWith("WP") || tags(dep.destination).startsWith("WD")) &&
                head < dep.destination && dep.destination < relVerb) {
              dgi.addEdge(relVerb, head, label)
              dgi.removeEdge(dep.source, dep.destination, dep.relation)
              done = true
            }
          }
        }
      }
    }
  }
}
