package org.clulab.ie

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._


object Negation extends LazyLogging {

  val negLemmaPhrases = Set(
    "fail",
    "insufficient",
    "play no",
    "play little"
  )

  /**
    * Determines whether or not a [[org.clulab.odin.Mention]] is negated.
    */
  def isNegated(mention: Mention): Boolean = {

    // count the number of negations originating at the trigger and each arg
    val dependencyNegs: Int = countDependencyNegations(mention)

    val sentenceRepresentation = getSentenceRepresentation(mention)

    // count contextual negations (ex. "insufficient evidence of")
    val supplementalCount = for {
      term <- negLemmaPhrases
      // pad with whitespace to ensure match is never a sub-word match
      negTerm = s" $term "
      if sentenceRepresentation contains negTerm
    } yield 1

    // count the number of syntactic and contextual negations for a mention
    val negCounts = dependencyNegs + supplementalCount.sum

    val negated: Boolean = negCounts != 0 && negCounts % 2 != 0

    negated
  }

  // create a whitespace-delimited lemmatized string of from the sentence's tokens.
  // for any token indices that are a part of the mention's trigger or arg intervals,
  // use a mask (##) to represent the token.
  // The result is used for substring checks for negation terms
  def getSentenceRepresentation(mention: Mention): String = {
    // find indices of tokens outside of mention's trigger and arg intervals
    val indicesOutside = indicesOutsideMention(mention)
    val indices = mention.sentenceObj.words.indices
    val lemmas = mention.sentenceObj.lemmas.get

    val res = for (i <- indices) yield {
      // lemma or token mask
      if (indicesOutside contains i) lemmas(i) else "##"
    }

    // prepend whitespace to delimit words at sentence edges
    s" ${res.mkString(" ")} "
  }

  /**
    * Find token indices outside of the intervals for a mention's trigger or args
    */
  def indicesOutsideMention(m: Mention): Set[Int] = {

    def indicesOutsideArguments(m: Mention): Set[Int] = {
      m.arguments.values.flatten.flatMap(indicesOutsideMention).toSet
    }

    m match {
      case tb: TextBoundMention =>
        m.sentenceObj.words.indices.filterNot(i => m.tokenInterval contains i).toSet
      case event: EventMention =>
        val args = indicesOutsideArguments(event)
        val trigger = indicesOutsideMention(event.trigger)
        args.intersect(trigger)
      case rel: RelationMention => indicesOutsideArguments(rel)
    }
  }

  /**
    * Count syntactic dependency negations that directly connect to a mention's trigger or args
    */
  // FIXME: arg-based approach fails on "tobacco use may not be responsible for an increase in the prevalence of lung cancer"
  // Prolonged <mark class="mention mention-PositiveRegulation"><mark class="mention-arg mention-arg-controller">tobacco use</mark> may not be responsible for an <mark class="mention-trigger">increase</mark> in <mark class="mention-arg mention-arg-controlled">the prevalence of lung cancer</mark></mark> .
//  def countDependencyNegations(m: Mention, negEdges: Seq[(Int, Int, String)]): Int = {
//
//    def countNegDepsForArgs(m: Mention): Int = {
//      m.arguments.values.flatten.map(countDependencyNegations(_, negEdges)).sum
//    }
//
//    m match {
//      case tb: TextBoundMention =>
//        // src of triplet must match tok's index
//        tb.tokenInterval.count(s => negEdges.exists(triplet => triplet._1 == s))
//      case rel: RelationMention => countNegDepsForArgs(rel)
//      case event: EventMention =>
//        val argCount = countNegDepsForArgs(event)
//        argCount + countDependencyNegations(event.trigger, negEdges)
//    }
//  }
  def countDependencyNegations(m: Mention, negEdges: Seq[(Int, Int, String)]): Int = {
    m.tokenInterval.count(s => negEdges.exists(triplet => triplet._1 == s))
  }
  def countDependencyNegations(m: Mention): Int = {
    if (m.sentenceObj.dependencies.isDefined && m.sentenceObj.lemmas.isDefined) {
      val lemmas = m.sentenceObj.lemmas.get
      val deps = m.sentenceObj.dependencies.get
      // find syntactic negations, excluding "not only"
      val negEdges = {
        deps
          .allEdges
          .filter{ triplet =>
            val rel = triplet._3
            val source = triplet._1
            // check that this is not a case of "not only"
            val valid = if (source + 1 < m.sentenceObj.words.length) lemmas(source + 1) != "only" else true
            rel == "neg" && valid
          }
      }
      if (negEdges.isEmpty) 0 else countDependencyNegations(m, negEdges)
    } else {
      logger.error(s"No dependencies for sentence '${m.sentenceObj.getSentenceText()}'")
      0
    }
  }
}

