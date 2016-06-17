package org.clulab.processors

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.scalatest.{Matchers, FlatSpec}

/**
 *
 * User: mihais
 * Date: 9/19/15
 */
class TestShortestPath extends FlatSpec with Matchers {
  val pp = new FastNLPProcessor()

  "FastNLPProcessor" should "find a valid shortest dependency path" in {
    val text = "The government is watching closely to see if their presence in the townships leads to increased anti-government protests and violence ; if it does , Pretoria will use this as a reason to keep Mr. Mandela behind bars ."
    val pred = 3 // watching
    val arg = 0 // The

    val doc = pp.annotate(text)
    val sent = doc.sentences.head
    val deps = sent.dependencies.get
    println(deps)
    val paths = deps.shortestPathEdges(pred, arg, ignoreDirection = true)
    paths.isEmpty should be (false)
    val path = paths.head
    val dirPathLabels = path.map(d => s"${d._1}->${d._2}:${d._3}${d._4}").mkString("-")
    println(s"Shortest path: [$dirPathLabels]")
    path.isEmpty should be (false)
    dirPathLabels should be ("3->1:nsubj>-1->0:det>")
  }
}
