package edu.arizona.sista.matcher

import edu.arizona.sista.processors.{Document, Sentence}
import scala.collection.mutable.{HashMap, ArrayBuffer}

class State(val document: Document) {
  private val lookUpTable = new HashMap[(Int, Int), ArrayBuffer[Mention]]

  def update(mention: Mention) {
    for (i <- mention.tokenInterval.toRange) {
      val key = (mention.sentence, i)
      val mentions = lookUpTable.getOrElseUpdate(key, new ArrayBuffer[Mention])
      mentions += mention
    }
  }

  def update(mentions: Seq[Mention]) {
    mentions foreach update
  }

  def sentenceIndex(s: Sentence) = document.sentences.indexOf(s)

  def allMentions: Seq[Mention] = lookUpTable.values.toSeq.flatten.distinct

  def mentionsFor(sentence: Int, token: Int): Seq[Mention] = lookUpTable((sentence, token))
}
