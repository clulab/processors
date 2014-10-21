package edu.arizona.sista.matcher

import scala.collection.mutable.{HashMap, ArrayBuffer}

class State {
  val tokenToMentions = new HashMap[(Int, Int), ArrayBuffer[Mention]]

  def update(mention: Mention) {
    for (i <- mention.tokenInterval.toRange) {
      val key = (mention.sentence, i)
      val mentions = tokenToMentions.getOrElseUpdate(key, new ArrayBuffer[Mention])
      mentions += mention
    }
  }

  def update(mentions: Seq[Mention]) {
    mentions foreach update
  }
}
