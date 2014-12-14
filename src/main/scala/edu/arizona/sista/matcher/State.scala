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

  def mentionsForRule(sent: Int, tok: Int, ruleName: String): Seq[Mention] =
    mentionsFor(sent, tok) filter (_.foundBy == Some(ruleName))

  def mentionsFor(sent: Int, tok: Int): Seq[Mention] =
    lookUpTable.getOrElse((sent, tok), Nil)

  def mentionsFor(sent: Int, tok: Int, label: String): Seq[Mention] =
    mentionsFor(sent, tok) filter (_ matches label)

  def mentionsFor(sent: Int, tok: Int, labels: Seq[String]): Seq[Mention] =
    labels flatMap (l => mentionsFor(sent, tok, l))

  def mentionsFor(sent: Int, toks: Seq[Int], label: String): Seq[Mention] =
    toks flatMap (t => mentionsFor(sent, t, label))

  def mentionsFor(sent: Int, toks: Seq[Int], labels: Seq[String]): Seq[Mention] =
    toks flatMap (t => mentionsFor(sent, t, labels))
}
