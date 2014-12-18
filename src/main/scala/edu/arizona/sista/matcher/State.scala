package edu.arizona.sista.matcher

import edu.arizona.sista.processors.{Document, Sentence}
import scala.collection.mutable.{HashMap, ArrayBuffer}

class State {
  private val lookUpTable = new HashMap[(Int, Int), ArrayBuffer[Mention]]

  def update(mention: Mention) {
    for (i <- mention.tokenInterval.toSeq) {
      val key = (mention.sentence, i)
      val mentions = lookUpTable.getOrElseUpdate(key, new ArrayBuffer[Mention])
      mentions += mention
    }
  }

  def update(mentions: Seq[Mention]) {
    mentions foreach update
  }

  def allMentions: Seq[Mention] = lookUpTable.values.toSeq.flatten.distinct

  // checks if a mention is already contained in the state
  // it checks if it is the same mention AND it was produced by the same rule
  def contains(m: Mention): Boolean =
    mentionsFor(m.sentence, m.start) exists (x => x == m && x.foundBy == m.foundBy)

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
