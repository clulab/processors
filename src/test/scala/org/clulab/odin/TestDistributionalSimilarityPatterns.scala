package org.clulab.odin

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.scalatest.{FlatSpec, Matchers}


class TestDistributionalSimilarityPatterns extends FlatSpec with Matchers {

  val proc = new FastNLPProcessor

  // the rule file containing the path to the embeddings resources
  val mf = "src/test/resources/org/clulab/odin/grammars/embeddings.yml"

  def readFile(filename: String) = {
    val source = io.Source.fromFile(filename)
    val data = source.mkString
    source.close()
    data
  }

  val rules = readFile(mf)
  val ee = ExtractorEngine(rules)

  val felineText = "I saw a leopard climbing a tree."

  felineText should "produce a mention with the label \"Feline\" for the word \"leopards\"" in {
    val doc = proc annotate felineText
    val mentions = ee.extractFrom(doc)
    val canines = mentions filter(_ matches "Canine")
    val felines = mentions filter(_ matches "Feline")
    felines foreach (m => println(m.text))
    canines should be (empty)
    felines should have size (1)
  }
}
