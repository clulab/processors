package edu.arizona.sista.matcher

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document

class TokenPattern(val start: Inst) {
  def findPrefixOf(sent: Int, doc: Document): Option[Result] = findPrefixOf(0, sent, doc)

  def findPrefixOf(tok: Int, sent: Int, doc: Document): Option[Result] = {
    ThompsonVM.evaluate(start, tok, sent, doc) map { m =>
      val (start, end) = m(TokenPattern.GlobalCapture)
      Result(start, end, m - TokenPattern.GlobalCapture mapValues {
        case (from, until) => Interval(from, until)
      })
    }
  }

  def findFirstIn(sent: Int, doc: Document): Option[Result] = findFirstIn(0, sent, doc)

  def findFirstIn(tok: Int, sent: Int, doc: Document): Option[Result] = {
    val n = doc.sentences(sent).size
    for (i <- tok until n) {
      val r = findPrefixOf(i, sent, doc)
      if (r.isDefined) return r
    }
    None
  }

  // returns results with sentence index
  def findAllIn(doc: Document): Seq[(Result, Int)] = for {
    s <- 0 until doc.sentences.size
    r <- findAllIn(s, doc)
  } yield (r, s)

  def findAllIn(sent: Int, doc: Document): Seq[Result] = findAllIn(0, sent, doc)

  def findAllIn(tok: Int, sent: Int, doc: Document): Seq[Result] = {
    def loop(i: Int): Stream[Result] = findFirstIn(i, sent, doc) match {
      case None => Stream.empty
      case Some(r) => r #:: loop(r.end)
    }
    loop(tok)
  }
}

object TokenPattern {
  val GlobalCapture = "--GLOBAL--"

  def compile(input: String): TokenPattern = TokenPatternCompiler.compile(input)
}

case class Result(start: Int, end: Int, groups: Map[String, Interval])

object TokenPatternCompiler extends TokenPatternParsers {
  def compile(input: String): TokenPattern = parseAll(tokenPattern, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => sys.error(failure.msg)
  }
}
