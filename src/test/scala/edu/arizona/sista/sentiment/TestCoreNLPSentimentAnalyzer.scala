package edu.arizona.sista.sentiment

import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import org.scalatest.{Matchers, FlatSpec}


class TestCoreNLPSentimentAnalyzer extends FlatSpec with Matchers {

  val proc = new CoreNLPProcessor(withDiscourse = false)

  this.getClass.getSimpleName should "correctly assign sentiment scores for negative cases" in {
    val doc = proc.annotate("The grumpy goblin toiled away in the fetid mines.")
    val s = doc.sentences.head
    val score = CoreNLPSentimentAnalyzer.sentiment(s)
    score should be < (3)
  }

  it should "correctly assign sentiment scores for positive cases" in {
    val doc = proc.annotate("The majestic unicorn cantered across the rainbow.")
    val s = doc.sentences.head
    val score = CoreNLPSentimentAnalyzer.sentiment(s)
    score should be >= (3)
  }
}
