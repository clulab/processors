package org.clulab.processors

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.corenlp.parser.EvaluateUtils
import org.scalatest.{Matchers, FlatSpec}

/**
  * Tests for proper tokenization in the Bio domain
  * User: mihais
  * Date: 7/6/16
  */
class TestEvaluateUtils extends FlatSpec with Matchers {
  val proc: Processor = new FastNLPProcessor(withChunks = false)

  "parser.EvaluateUtils.evaluate()" should "produce a perfect score when a parse is compared to itself" in {

    val text = "I pee freely."
    val doc = proc.annotate(text)
    val gold1 = doc.sentences.head.stanfordBasicDependencies.get
    val performance1 = EvaluateUtils.evaluate(gold1, gold1, withEdgeLabel = false)

    val perfectScore = 1.0

    performance1.precision should equal (perfectScore)
    performance1.recall should equal (perfectScore)
    performance1.f1 should equal (perfectScore)

    val performance2 = EvaluateUtils.evaluate(gold1, gold1, withEdgeLabel = true)

    performance2.precision should equal (perfectScore)
    performance2.recall should equal (perfectScore)
    performance2.f1 should equal (perfectScore)


    val gold2 = doc.sentences.head.stanfordCollapsedDependencies.get
    val performance3 = EvaluateUtils.evaluate(gold2, gold2, withEdgeLabel = false)

    performance3.precision should equal (perfectScore)
    performance3.recall should equal (perfectScore)
    performance3.f1 should equal (perfectScore)

    val performance4 = EvaluateUtils.evaluate(gold2, gold2, withEdgeLabel = true)

    performance4.precision should equal (perfectScore)
    performance4.recall should equal (perfectScore)
    performance4.f1 should equal (perfectScore)
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
}