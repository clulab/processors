package org.clulab.odin.impl

import org.clulab.struct.Interval
import org.clulab.processors.Document
import org.clulab.odin._

object TokenPattern {
  val GlobalCapture = "--GLOBAL--"

  def compile(
      input: String,
      unit: String = "word",
      config: OdinConfig = OdinConfig.empty
  ): TokenPattern = {
    val compiler = new TokenPatternParsers(unit, config)
    compiler.compileTokenPattern(input)
  }

  // returns true if the next `Match*` instruction is a `MatchMention`
  def startsWithMatchMention(inst: Inst): Boolean = inst match {
    case i: MatchMention => true
    case i: Pass => startsWithMatchMention(i.next)
    case i: Split => startsWithMatchMention(i.lhs) && startsWithMatchMention(i.rhs)
    case i: SaveStart => startsWithMatchMention(i.next)
    case i: SaveEnd => startsWithMatchMention(i.next)
    case _ => false
  }

  case class Result(
      interval: Interval,
      groups: Map[String, Seq[Interval]],
      mentions: Map[String, Seq[Mention]]
  ) {
    val start = interval.start
    val end = interval.end
  }
}

class TokenPattern(val start: Inst) {
  import TokenPattern._

  // We want to advance token by token when the first match in the pattern is a MatchMention.
  // This allows us to find overlapping mentions that start in different tokens but end in the same one.
  // e.g. [ASPP1 and [ASPP2 are phosphorylated]] in response to EGFR
  val cautiousAdvance = startsWithMatchMention(start)

  def findPrefixOf(tok: Int, sent: Int, doc: Document, state: State): Seq[Result] = {
    ThompsonVM.evaluate(start, tok, sent, doc, state) map {
      case (groups, mentions) =>
        // there must be one GlobalCapture only
        val globalCapture = groups(GlobalCapture).head
        Result(globalCapture, groups - GlobalCapture, mentions)
    }
  }

  def findFirstIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Result] = {
    val n = doc.sentences(sent).size
    for (i <- tok until n) {
      val r = findPrefixOf(i, sent, doc, state)
      if (r.nonEmpty) return r
    }
    Nil
  }

  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Result] = {
    @annotation.tailrec
    def collect(i: Int, collected: Seq[Result]): Seq[Result] = {
      findFirstIn(i, sent, doc, state) match {
        case Nil => collected
        case results =>
          val r = results minBy (_.interval.size)
          collect(r.end, collected ++ results)
      }
    }
    if (cautiousAdvance) {
      // move forward one token at a time
      val n = doc.sentences(sent).size
      for {
        i <- tok until n // the rest of the tokens
        r <- findPrefixOf(i, sent, doc, state)
      } yield r
    } else {
      // move forward one match at a time
      collect(tok, Nil)
    }
  }

  def findPrefixOf(sent: Int, doc: Document, state: State = new State): Seq[Result] =
    findPrefixOf(0, sent, doc, state)

  def findFirstIn(sent: Int, doc: Document, state: State = new State): Seq[Result] =
    findFirstIn(0, sent, doc, state)

  def findAllIn(sent: Int, doc: Document, state: State = new State): Seq[Result] =
    findAllIn(0, sent, doc, state)

}
