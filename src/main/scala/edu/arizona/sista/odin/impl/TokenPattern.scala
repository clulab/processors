package edu.arizona.sista.odin.impl

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin._

object TokenPattern {
  val GlobalCapture = "--GLOBAL--"

  def compile(input: String): TokenPattern = TokenPatternCompiler.compile(input)

  case class Result(
      interval: Interval,
      groups: Map[String, Interval],
      mentions: Map[String, Mention]
  ) {
    val start = interval.start
    val end = interval.end
  }
}

class TokenPattern(val start: Inst, val lookahead: Option[TokenPatternLookaheadAssertion]) {
  import TokenPattern._

  def findPrefixOf(tok: Int, sent: Int, doc: Document, state: Option[State]): Seq[Result] = {
    val results = ThompsonVM.evaluate(start, tok, sent, doc, state) map {
      case (groups, mentions) =>
        val (start, end) = groups(GlobalCapture)
        val newGroups = groups - GlobalCapture transform {
          case (name, (from, until)) => Interval(from, until)
        }
        Result(Interval(start, end), newGroups, mentions)
    }
    lookahead match {
      case None => results
      case Some(assertion) => assertion.filter(results, sent, doc, state)
    }
  }

  def findFirstIn(tok: Int, sent: Int, doc: Document, state: Option[State]): Seq[Result] = {
    val n = doc.sentences(sent).size
    for (i <- tok until n) {
      val r = findPrefixOf(i, sent, doc, state)
      if (r.nonEmpty) return r
    }
    Nil
  }

  def findAllIn(tok: Int, sent: Int, doc: Document, state: Option[State]): Seq[Result] = {
    @annotation.tailrec
    def collect(i: Int, collected: Seq[Result]): Seq[Result] =
      findFirstIn(i, sent, doc, state) match {
        case Nil => collected
        case results =>
          val r = results minBy (_.interval.size)
          collect(r.end, results ++ collected)
      }
    collect(tok, Nil)
  }

  def findPrefixOf(tok: Int, sent: Int, doc: Document, state: State): Seq[Result] =
    findPrefixOf(tok, sent, doc, Some(state))

  def findPrefixOf(sent: Int, doc: Document, state: Option[State]): Seq[Result] =
    findPrefixOf(0, sent, doc, state)

  def findPrefixOf(sent: Int, doc: Document, state: State): Seq[Result] =
    findPrefixOf(sent: Int, doc: Document, Some(state))

  def findFirstIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Result] =
    findFirstIn(tok, sent, doc, Some(state))

  def findFirstIn(sent: Int, doc: Document, state: Option[State]): Seq[Result] =
    findFirstIn(0, sent, doc, state)

  def findFirstIn(sent: Int, doc: Document, state: State): Seq[Result] =
    findFirstIn(sent, doc, Some(state))

  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Result] =
    findAllIn(tok, sent, doc, Some(state))

  def findAllIn(sent: Int, doc: Document, state: Option[State]): Seq[Result] =
    findAllIn(0, sent, doc, state)

  def findAllIn(sent: Int, doc: Document, state: State): Seq[Result] =
    findAllIn(sent, doc, Some(state))
}

class TokenPatternLookaheadAssertion(val start: Inst, val negative: Boolean) {
  def filter(
    results: Seq[TokenPattern.Result],
    sent: Int,
    doc: Document,
    state: Option[State]
  ): Seq[TokenPattern.Result] = results filter { r =>
    val results = ThompsonVM.evaluate(start, r.end, sent, doc, state)
    negative == results.isEmpty
  }
}
