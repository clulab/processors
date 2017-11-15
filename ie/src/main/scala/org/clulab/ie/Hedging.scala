package org.clulab.ie

import org.clulab.odin._
import org.clulab.struct.DirectedGraph


/**
  * Detects hedging.
  * Based on org.clulab.reach.darpa.HypothesisHandler
  */
object Hedging {

  def isHedged(m: Mention): Boolean = detectHedging(m)

  private def detectHedging(m: Mention): Boolean = {

    val degree = 2 // Degree up to which we should follow the links in the graph

    val hedgingTerms = Set(
      "argue",
      "argument",
      "believe",
      "belief",
      "conjecture",
      "consider",
      "hint",
      "hypothesis",
      "hypotheses",
      "hypothesize",
      "implication",
      "imply",
      "indicate",
      "may",
      "predict",
      "prediction",
      "previous",
      "previously",
      "proposal",
      "propose",
      "question",
      "speculate",
      "speculation",
      "suggest",
      "suspect",
      "theorize",
      "theory",
      "think",
      "whether")

    m match {
      case tb: TextBoundMention => false
      // check events and relations
      case m: Mention =>
        // Get the dependencies of the sentence
        val dependencies = m.sentenceObj.dependencies.getOrElse(new DirectedGraph[String](Nil, Set[Int]()))

        val mentionInterval: Seq[Int] = m.tokenInterval

        // Get the index of the word outside the event up to "degree" degrees
        val expandedIndices: Seq[Int] = mentionInterval flatMap (getIndices(_, degree, dependencies))

        // Remove duplicates
        val indices: Seq[Int] = (mentionInterval ++ expandedIndices).distinct

        // Get the lemmas
        val lemmas = indices map (m.sentenceObj.lemmas.get(_))

        // Detect hedging
        lemmas.exists(lemma => hedgingTerms contains lemma)

    }

  }

  // Recursive function that helps us get the words outside the event
  private def getIndices(
    currentIndex: Int,
    degree: Int,
    dependencies: DirectedGraph[String]
  ): Seq[Int] = degree match {
    case 0 => Seq[Int]() // Base case of the recursion
    case _ =>

      val outgoing = dependencies.outgoingEdges
      val incoming = dependencies.incomingEdges

      // Get incoming and outgoing edges
      val t:Seq[(Int, String)] = incoming.lift(currentIndex)  match {
        case Some(x) => x
        case None => Seq()
      }

      val edges = t ++ (outgoing.lift(currentIndex) match {
        case Some(x) => x
        case None => Seq()
      })

      // Each edge is a tuple of (endpoint index, edge label), so we map it to the first
      // element of the tuple
      val indices: Seq[Int] = edges map (_._1)

      // Recursively call this function to get outer degrees
      val res = indices flatMap (i => getIndices(i, degree - 1, dependencies))
      (res ++ indices).distinct
  }

}
