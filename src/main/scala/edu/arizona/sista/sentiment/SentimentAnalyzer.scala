package edu.arizona.sista.sentiment

import edu.arizona.sista.processors.{Sentence, Document}

/**
 * skeleton for sentiment analysis
 */
trait SentimentAnalyzer {

  def sentiment(doc: Document)
  def sentiment(s: Sentence)
  def sentiment(text: String)

}
