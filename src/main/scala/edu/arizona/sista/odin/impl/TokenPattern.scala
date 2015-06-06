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

class TokenPattern(
    val start: Inst,
    val lookbehind: Option[LookbehindAssertion],
    val lookahead: Option[LookaheadAssertion]
) {
  import TokenPattern._

  def findPrefixOf(tok: Int, sent: Int, doc: Document, state: Option[State]): Seq[Result] = {
    // enforce lookbehind assertion if there is one
    if (lookbehind.isDefined && !lookbehind.get.matches(tok, sent, doc, state))
      return Nil
    // apply the main pattern
    val results = ThompsonVM.evaluate(start, tok, sent, doc, state) map {
      case (groups, mentions) =>
        val (start, end) = groups(GlobalCapture)
        val newGroups = groups - GlobalCapture transform {
          case (name, (from, until)) => Interval(from, until)
        }
        Result(Interval(start, end), newGroups, mentions)
    }
    // enforce lookahead assertion if there is one
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
          collect(r.end, collected ++ results)
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

class LookbehindAssertion(val start: Inst, val size: Int, val negative: Boolean) {
  def matches(tok: Int, sent: Int, doc: Document, state: Option[State]): Boolean = {
    val startTok = tok - size
    if (startTok < 0) negative // only fail if negative is false
    else {
      val results = ThompsonVM.evaluate(start, startTok, sent, doc, state)
      negative == results.isEmpty
    }
  }
}

class LookaheadAssertion(val start: Inst, val negative: Boolean) {
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
