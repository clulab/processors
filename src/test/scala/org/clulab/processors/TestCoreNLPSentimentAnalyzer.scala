package org.clulab.processors

import org.clulab.processors.corenlp.{CoreNLPSentimentAnalyzer, CoreNLPProcessor}
import org.scalatest.{FlatSpec, Matchers}


class TestCoreNLPSentimentAnalyzer extends FlatSpec with Matchers {

  val proc = new CoreNLPProcessor()

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
