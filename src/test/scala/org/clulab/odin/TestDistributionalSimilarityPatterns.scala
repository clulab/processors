package org.clulab.odin

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.scalatest.{FlatSpec, Matchers}
import TestUtils._


class TestDistributionalSimilarityPatterns extends FlatSpec with Matchers {

  val proc = new FastNLPProcessor

  val rules = readFile(embeddingsFile)
  val ee = ExtractorEngine(rules)

  val felineText = "I saw a leopard climbing a tree."

  felineText should "produce a mention with the label \"Feline\" for the word \"leopard\"" in {
    val doc = proc annotate felineText
    val mentions = ee.extractFrom(doc)
    val canines = mentions filter(_ matches "Canine")
    val felines = mentions filter(_ matches "Feline")
    felines foreach (m => println(m.text))
    // we shouldn't find any Canines
    canines should be (empty)
    felines should have size (1)
    felines.head.text should equal("leopard")
  }

}
