package org.clulab.utils

import org.clulab.processors.Sentence
import org.clulab.struct.{ DirectedGraph, Interval }

/**
 * Utility functions for use with directed (dependency) graphs
 * User: danebell
 * Date: 2/23/15
 */
object DependencyUtils {

  // a dependency graph is a directed graph with strings as edges
  type DependencyGraph = DirectedGraph[String]

  // a policy is a function that chooses an int from a sequence
  type Policy = Seq[Int] => Int

  // get all the governors of the given token according to the dependency graph
  def followIncoming(tok: Int, graph: DependencyGraph): Array[Int] = graph.incomingEdges.lift(tok).getOrElse(Array.empty).map(_._1)

  // by default we choose the last element of the sequence
  val defaultPolicy: Policy = _.last

  /**
   * Given an Interval, finds the minimal span covering all of the Interval's nodes' children (recursively).
   *
   * @param span Interval of nodes
   * @param sent the sentence over which the interval applies
   * @return the minimal Interval that contains all the nodes that are children of span's nodes
   */
  def subgraph(span: Interval, sent: Sentence): Option[Interval] = {
    val graph = sent.dependencies.getOrElse(return None)

    val heads = if (span.size < 2) Seq(span.start) else findHeadsStrict(span, sent)

    @annotation.tailrec
    def followTrail(remaining: Seq[Int], results: Seq[Int]): Seq[Int] = remaining match {
      case Nil => results
      case first +: rest if results contains first => followTrail(rest, results)
      case first +: rest =>
        val children: Seq[Int] = try {
          graph.getOutgoingEdges(first).map(_._1)
        } catch {
          case e: Exception =>
            Nil
        }
        followTrail(children ++ rest, first +: results)
    }

    val outgoing = (for (h <- heads) yield followTrail(Seq(h), Nil)).flatten.distinct

    // outgoing may only have a single index
    if (outgoing.length > 1) Some(Interval(outgoing.min, outgoing.max+1)) else Some(Interval(outgoing.min, outgoing.min + 1))
  }

  /**
   * Finds the highest node (i.e. closest to a root) in an Interval of a directed graph. If there are multiple nodes of
   * the same rank, chooseWhich adjudicates which single node is returned.
   *
   * @param span an Interval of nodes
   * @param graph a directed graph containing the nodes in span
   * @param chooseWhich a function deciding which of multiple heads is returned; the rightmost head selected by default
   * @return the single node which is closest to the root among those in span
   */
  def findHead(span: Interval, graph: DependencyGraph, chooseWhich: Policy = defaultPolicy): Int = {
    chooseWhich(findHeads(span, graph))
  }

  /**
   * Finds the highest node (i.e. closest to a root) in an Interval of a directed graph. If there are multiple nodes of
   * the same rank, all are returned.
   *
   * @param span an Interval of nodes
   * @param graph a directed graph containing the nodes in span
   * @return the single node which is closest to the root among those in span
   */
  def findHeads(span: Interval, graph: DependencyGraph): Seq[Int] = {
    @annotation.tailrec
    def countSteps(toksWithDist: List[(Int, Double)], seen: Set[Int]): Double = toksWithDist match {
      case Nil =>
        // we couldn't find a root in the graph
        // maybe it is not a valid dependency graph?
        throw new DependencyUtilsException("can't find a root")
      case (tok, dist) :: rest if seen contains tok =>
        // we already explored this token, skip
        countSteps(rest, seen)
      case (tok, dist) :: rest if graph.roots contains tok =>
        // found a root
        // it is the closest one because we are searching breath-first
        // return distance
        dist
      case (tok, dist) :: rest =>
        // explore
        val incoming = followIncoming(tok, graph)
        if (incoming.isEmpty) {
          // this token has no incomings, but it is not a root
          // it looks like a collapsed dependency graph
          // (it couldn't be farther from the root)
          Double.PositiveInfinity
        } else {
          // keep looking, breadth-first
          val nextStep = incoming.map(i => (i, dist + 1)).toList
          countSteps(rest ::: nextStep, seen + tok)
        }
    }

    // returns the distance to the closest root for a given token
    def distToRoot(token: Int): Double = countSteps(List((token, 0)), Set.empty)

    // get the distance to root for each token in span
    val toksWithDist = span.map(t => (t, distToRoot(t)))
    val dists = toksWithDist.map(_._2)
    if (dists.isEmpty) {
      Nil
    } else {
      // return all tokens with minimum distance
      val minDist = dists.min
      for ((t, d) <- toksWithDist if d == minDist) yield t
    }
  }

  /**
   * Find the single highest node in an interval of a dependency graph, ignoring punctuation, coordinations, and
   * prepositions.
   * @param span the interval within which to search
   * @param sent the Sentence within which to look
   * @param chooseWhich the function to adjudicate which is highest when there's a tie
   * @return Option containing the highest node index, or None if no such node is found
   */
  def findHeadStrict(span: Interval, sent: Sentence, chooseWhich: Policy = defaultPolicy): Option[Int] = {
    val hds = findHeadsStrict(span, sent)
    hds match {
      case Nil => None
      case heads => Some(chooseWhich(heads))
    }
  }

  /**
   * Find the highest nodes in an interval of a dependency graph, ignoring puncutation, coordinations, and prepositions.
   * Allows multiple node indices to be "highest" in the case of a tie.
   * @param span the interval within which to search
   * @param sent the Sentence within which to look
   * @return Option containing a sequence of highest node indices, or None if no such node is found
   */
  def findHeadsStrict(span: Interval, sent: Sentence): Seq[Int] = sent.dependencies match {
    case None => Nil
    case Some(graph) =>
      val stopTags = "(.|,|\\(|\\)|:|''|``|#|$|CC|TO|IN)"
      val heads = findHeads(span, graph)
      heads.filter(x => !(sent.tags.get(x) matches stopTags))
  }


  /**
   * Finds the highest node (i.e. closest to a root) in an Interval of a directed graph, ignoring the graph outside the
   * Interval. If there are multiple nodes of the same rank, chooseWhich adjudicates which single node is returned.
   * <p>
   * Crucially, any node that has an incoming edge from outside the Interval is considered a head. This is efficient if
   * you know your span has a single head that is also within the span.
   *
   * @param span an Interval of nodes
   * @param graph a directed graph containing the nodes in span
   * @param chooseWhich a function deciding which of multiple heads is returned; the rightmost head selected by default
   * @return the single node which is closest to the root among those in span
   */
  def findHeadLocal(span: Interval, graph: DependencyGraph, chooseWhich: Policy = defaultPolicy): Int = {
    chooseWhich(findHeadsLocal(span, graph))
  }

  /**
   * Finds the highest node (i.e. closest to a root) in an Interval of a directed graph. If there are multiple nodes of
   * the same rank, all are returned.
   * <p>
   * Crucially, any node that has an incoming edge from outside the Interval is considered a head. This is efficient if
   * you know your span has a single head that is also within the span.
   *
   * @param span an Interval of nodes
   * @param graph a directed graph containing the nodes in span
   * @return the single node which is closest to the root among those in span
   */
  def findHeadsLocal(span: Interval, graph: DependencyGraph): Seq[Int] = {

    def followTrail (i: Int, heads:Seq[Int]): Seq[Int] = {

      // dependents
      val incoming = graph.getIncomingEdges(i).map(_._1)

      incoming match {
        case valid if valid.isEmpty | valid.contains(i) =>  Seq(i)
        case found if heads.contains(i) | found.min < span.start | found.max > (span.end - 1) => Seq(i)
        case _ => followTrail(incoming.head, heads ++ Seq(i))
      }
    }

    val heads = for (i <- span.start until span.end) yield followTrail(i, Nil)

    heads.flatten.distinct.toSeq.sorted
  }

  /**
   *
   * @param a Interval in Sentence sentA
   * @param b Interval in Sentence sentB
   * @param sentA Sentence containing a
   * @param sentB Sentence containing b
   * @return returns true if Interval a contains Interval b or vice versa
   */
  def nested(a: Interval, b: Interval, sentA: Sentence, sentB: Sentence): Boolean = {
    if (sentA != sentB) return false

    val graph = sentA.dependencies.getOrElse(return false)

    val aSubgraph = subgraph(a, sentA)
    val bSubgraph = subgraph(b, sentB)

    if((aSubgraph.isDefined && aSubgraph.get.contains(b)) ||
      (bSubgraph.isDefined && bSubgraph.get.contains(a))) true
    else false
  }
}

/** Exception thrown by errors in DependencyUtils */
class DependencyUtilsException(message: String = null, cause: Throwable = null)
extends RuntimeException(message, cause)
