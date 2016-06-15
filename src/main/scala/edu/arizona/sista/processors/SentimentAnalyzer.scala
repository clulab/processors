package edu.arizona.sista.processors

/**
 * skeleton for sentiment analysis
 */
trait SentimentAnalyzer {

  def sentiment(doc: Document)
  def sentiment(s: Sentence)
  def sentiment(text: String)

}
