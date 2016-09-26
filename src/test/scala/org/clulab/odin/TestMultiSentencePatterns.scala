package org.clulab.odin

import org.scalatest._
import org.clulab.processors.shallownlp.ShallowNLPProcessor


class TestMultiSentencePatterns extends FlatSpec with Matchers {

  val text1 = "Barack Obama is the 44th President of the United States. He was born on August 4, 1961 in Honolulu, Hawaii."
  val proc = new ShallowNLPProcessor
  val rule =
    """
      |- name: coref-example
      |  label: Coref
      |  priority: 2
      |  # maximum sentential distance of args
      |  window: 1
      |  example: "Barack Obama is the 44th President of the United States. He was born on August 4, 1961 in Honolulu, Hawaii."
      |  type: cross-sentence
      |  pattern: |
      |    antecedent: Entity = Barack Obama
      |    # the pronoun to be resolved
      |    anaphor: Entity = [lemma="he"]
    """.stripMargin

  text1 should "produce a RelationMention with the label \"Coref\"" in {
    val doc = proc.annotate(text1)
    val ee = ExtractorEngine(rule)
    val mentions = ee.extractFrom(doc)
    mentions should have size (1)
    (mentions.head matches "Coref") should be (true)
  }

}
