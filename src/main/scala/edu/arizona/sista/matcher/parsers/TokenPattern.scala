package edu.arizona.sista.matcher

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document

class TokenPattern(val start: Inst) {
  import TokenPattern._

  def findPrefixOf(sent: Int, doc: Document): Option[Result] =
    findPrefixOf(sent, doc, None)

  def findPrefixOf(sent: Int, doc: Document, state: State): Option[Result] =
    findPrefixOf(sent, doc, Some(state))

  def findPrefixOf(sent: Int, doc: Document, state: Option[State]): Option[Result] =
    findPrefixOf(0, sent, doc, state)

  def findPrefixOf(tok: Int, sent: Int, doc: Document): Option[Result] =
    findPrefixOf(tok, sent, doc, None)

  def findPrefixOf(tok: Int, sent: Int, doc: Document, state: State): Option[Result] =
    findPrefixOf(tok, sent, doc, Some(state))

  def findPrefixOf(tok: Int, sent: Int, doc: Document, state: Option[State]): Option[Result] = {
    ThompsonVM.evaluate(start, tok, sent, doc, state) map { m =>
      val (start, end) = m(GlobalCapture)
      Result(Interval(start, end), m - GlobalCapture mapValues {
        case (from, until) => Interval(from, until)
      })
    }
  }

  def findFirstIn(sent: Int, doc: Document): Option[Result] =
    findFirstIn(sent, doc, None)

  def findFirstIn(sent: Int, doc: Document, state: State): Option[Result] =
    findFirstIn(sent, doc, Some(state))

  def findFirstIn(sent: Int, doc: Document, state: Option[State]): Option[Result] =
    findFirstIn(0, sent, doc, state)

  def findFirstIn(tok: Int, sent: Int, doc: Document): Option[Result] =
    findFirstIn(tok, sent, doc, None)

  def findFirstIn(tok: Int, sent: Int, doc: Document, state: State): Option[Result] =
    findFirstIn(tok, sent, doc, Some(state))

  def findFirstIn(tok: Int, sent: Int, doc: Document, state: Option[State]): Option[Result] = {
    val n = doc.sentences(sent).size
    for (i <- tok until n) {
      val r = findPrefixOf(i, sent, doc, state)
      if (r.isDefined) return r
    }
    None
  }

  // returns results with sentence index
  def findAllIn(doc: Document): Seq[(Result, Int)] =
    findAllIn(doc, None)

  def findAllIn(doc: Document, state: State): Seq[(Result, Int)] =
    findAllIn(doc, Some(state))

  def findAllIn(doc: Document, state: Option[State]): Seq[(Result, Int)] = for {
    s <- 0 until doc.sentences.size
    r <- findAllIn(s, doc, state)
  } yield (r, s)

  def findAllIn(sent: Int, doc: Document): Seq[Result] =
    findAllIn(sent, doc, None)

  def findAllIn(sent: Int, doc: Document, state: State): Seq[Result] =
    findAllIn(sent, doc, Some(state))

  def findAllIn(sent: Int, doc: Document, state: Option[State]): Seq[Result] =
    findAllIn(0, sent, doc, state)

  def findAllIn(tok: Int, sent: Int, doc: Document): Seq[Result] =
    findAllIn(tok, sent, doc, None)

  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Result] =
    findAllIn(tok, sent, doc, Some(state))

  def findAllIn(tok: Int, sent: Int, doc: Document, state: Option[State]): Seq[Result] = {
    def loop(i: Int): Stream[Result] = findFirstIn(i, sent, doc, state) match {
      case None => Stream.empty
      case Some(r) => r #:: loop(r.end)
    }
    loop(tok)
  }
}

object TokenPattern {
  val GlobalCapture = "--GLOBAL--"
  def compile(input: String): TokenPattern = TokenPatternCompiler.compile(input)
  case class Result(interval: Interval, groups: Map[String, Interval]) {
    val start = interval.start
    val end = interval.end
  }
}

object TokenPatternCompiler extends TokenPatternParsers {
  def compile(input: String): TokenPattern = parseAll(tokenPattern, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => sys.error(failure.msg)
  }
}
