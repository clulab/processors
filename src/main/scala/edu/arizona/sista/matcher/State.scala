package edu.arizona.sista.matcher

class State(val paperId: String, val section: String, val mentions: Seq[Mention]) {
  val tokenToMention = mentions flatMap { m =>
    m.tokenInterval.toRange map (i => (m.sentence, i) -> m)
  } groupBy (_._1) mapValues (_.map(_._2).toSet)
}
