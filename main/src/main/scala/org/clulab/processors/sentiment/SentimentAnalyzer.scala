package org.clulab.processors.sentiment

import org.clulab.processors.{Document, Sentence}

/**
 * skeleton for sentiment analysis
 */
trait SentimentAnalyzer {

  def sentiment(doc: Document)
  def sentiment(s: Sentence)
  def sentiment(text: String)

}
