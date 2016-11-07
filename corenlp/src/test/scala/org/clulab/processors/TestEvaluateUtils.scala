package org.clulab.processors

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.corenlp.parser.EvaluateUtils
import org.clulab.struct.{DirectedGraph, Edge}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Tests for proper tokenization in the Bio domain
  * User: mihais
  * Date: 7/6/16
  */
class TestEvaluateUtils extends FlatSpec with Matchers {
  val proc: Processor = new FastNLPProcessor(withChunks = false)
  val PERFECT = 1.0

  "parser.EvaluateUtils.evaluate()" should "produce a perfect score when a parse is compared to itself" in {

    val text = "I pee freely."
    val doc = proc.annotate(text)
    val gold1 = doc.sentences.head.stanfordBasicDependencies.get
    val performance1 = EvaluateUtils.evaluate(gold1, gold1, withEdgeLabel = false)


    performance1.precision should equal (PERFECT)
    performance1.recall should equal (PERFECT)
    performance1.f1 should equal (PERFECT)

    val performance2 = EvaluateUtils.evaluate(gold1, gold1, withEdgeLabel = true)

    performance2.precision should equal (PERFECT)
    performance2.recall should equal (PERFECT)
    performance2.f1 should equal (PERFECT)


    val gold2 = doc.sentences.head.stanfordCollapsedDependencies.get
    val performance3 = EvaluateUtils.evaluate(gold2, gold2, withEdgeLabel = false)

    performance3.precision should equal (PERFECT)
    performance3.recall should equal (PERFECT)
    performance3.f1 should equal (PERFECT)

    val performance4 = EvaluateUtils.evaluate(gold2, gold2, withEdgeLabel = true)

    performance4.precision should equal (PERFECT)
    performance4.recall should equal (PERFECT)
    performance4.f1 should equal (PERFECT)
  }

  "Performance + Performance" should "produce a valid result" in {

    val text = "I pee freely."
    val doc = proc.annotate(text)
    val gold1 = doc.sentences.head.stanfordBasicDependencies.get
    val performance1 = EvaluateUtils.evaluate(gold1, gold1, withEdgeLabel = false)

    val performance = performance1 + performance1

    performance.precision should equal(performance1.precision)
    performance.recall should equal(performance1.recall)
    performance.f1 should equal(performance1.f1)
  }

  "withEdgeLabel = true" should "produce a different score when source destination match, but the relation differs" in {

    val gold = DirectedGraph[String](List(Edge[String](0, 1, "a"), Edge[String](1, 2, "b"), Edge[String](2, 3, "c")), roots=Set(0))
    val predicted = DirectedGraph[String](List(Edge[String](0, 1, "a"), Edge[String](1, 2, "b"), Edge[String](2, 3, "d")), roots=Set(0))

    val per1 = EvaluateUtils.evaluate(gold, predicted, withEdgeLabel = false)

    per1.precision should equal (PERFECT)
    per1.recall should equal (PERFECT)
    per1.f1 should equal (PERFECT)

    val per2 = EvaluateUtils.evaluate(gold, predicted, withEdgeLabel = true)

    per2.precision should not equal (PERFECT)
    per2.recall should not equal (PERFECT)
    per2.f1 should not equal (PERFECT)
  }
}