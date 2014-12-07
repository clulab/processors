package edu.arizona.sista.matcher

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.{Sentence, Document}



object ProgCompiler extends TokenConstraintParsers {
  def compile(input: String): Prog = parseAll(pattern, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => sys.error(failure.msg)
  }

  def tokenPattern: Parser[Frag] = (wordConstraint | tokenConstraint) ^^ {
    case constraint => Frag(Match(constraint))
  }

  def capturePattern: Parser[Frag] = "(?<" ~ ident ~ ">" ~ splitPattern ~ ")" ^^ {
    case _ ~ name ~ _ ~ frag ~ _ => frag.capture(name)
  }

  def atomicPattern: Parser[Frag] = tokenPattern | capturePattern | "(" ~> splitPattern <~ ")"

  def repeatedPattern: Parser[Frag] = atomicPattern ~ ("??"|"*?"|"+?"|"?"|"*"|"+") ^^ {
    case frag ~ "?" => frag.greedyOptional
    case frag ~ "??" => frag.lazyOptional
    case frag ~ "*" => frag.greedyKleene
    case frag ~ "*?" => frag.lazyKleene
    case frag ~ "+" => frag.greedyPlus
    case frag ~ "+?" => frag.lazyPlus
  }

  def rangePattern: Parser[Frag] = atomicPattern ~ "{" ~ int ~ "," ~ int ~ ("}?"|"}") ^^ {
    case frag ~ _ ~ from ~ _ ~ to ~ "}" => frag.greedyRange(Some(from), Some(to))
    case frag ~ _ ~ from ~ _ ~ to ~ "}?" => frag.lazyRange(Some(from), Some(to))
  }

  def fromPattern: Parser[Frag] = atomicPattern ~ "{" ~ int ~ "," ~ ("}?"|"}") ^^ {
    case frag ~ _ ~ from ~ _ ~ "}" => frag.greedyRange(Some(from), None)
    case frag ~ _ ~ from ~ _ ~ "}?" => frag.lazyRange(Some(from), None)
  }

  def toPattern: Parser[Frag] = atomicPattern ~ "{" ~ "," ~ int ~ ("}?"|"}") ^^ {
    case frag ~ _ ~ _ ~ to ~ "}" => frag.greedyRange(None, Some(to))
    case frag ~ _ ~ _ ~ to ~ "}?" => frag.lazyRange(None, Some(to))
  }

  def exactPattern: Parser[Frag] = atomicPattern ~ "{" ~ int ~ "}" ^^ {
    case frag ~ _ ~ n ~ _ => frag.repeatPattern(n)
  }

  def quantifiedPattern: Parser[Frag] =
    repeatedPattern | rangePattern | fromPattern | toPattern | exactPattern | atomicPattern

  def concatPattern: Parser[Frag] = quantifiedPattern ~ rep(quantifiedPattern) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => Frag(lhs, rhs)
    }
  }

  def splitPattern: Parser[Frag] = concatPattern ~ rep("|" ~> concatPattern) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) =>
        val split = Split(lhs.in, rhs.in)
        Frag(split, lhs.out ++ rhs.out)
    }
  }

  def pattern: Parser[Prog] = splitPattern ^^ {
    case frag =>
      val f = frag.capture(Prog.GlobalCaptureName)
      f.setOut(Done)
      new Prog(f.in)
  }
}



class Frag(val in: Inst, val out: Seq[Inst]) {
  def setOut(inst: Inst) {
    out foreach { o =>
      o match {
        case i: Match => i.next = inst
        case i: Jump => i.next = inst
        case i: SaveStart => i.next = inst
        case i: SaveEnd => i.next = inst
        case _ => ()
      }
    }
  }

  def findOut(i: Inst): Seq[Inst] = i match {
    case Split(lhs, rhs) => findOut(lhs) ++ findOut(rhs)
    case i if i.next == null => Seq(i)
    case i => findOut(i.next)
  }

  def dup: Frag = {
    val newIn = in.dup
    Frag(newIn, findOut(newIn))
  }

  def repeat(n: Int): Seq[Frag] = {
    for (i <- 0 until n) yield dup
  }

  def capture(name: String): Frag = {
    val start = SaveStart(name)
    val end = SaveEnd(name)
    start.next = in
    setOut(end)
    Frag(start, Seq(end))
  }

  def greedyOptional: Frag = {
    val epsilon = Jump()
    val split = Split(in, epsilon)
    Frag(split, epsilon +: out)
  }

  def lazyOptional: Frag = {
    val epsilon = Jump()
    val split = Split(epsilon, in)
    Frag(split, epsilon +: out)
  }

  def greedyKleene: Frag = {
    val epsilon = Jump()
    val split = Split(in, epsilon)
    val jump = Jump()
    jump.next = split
    setOut(jump)
    Frag(split, Seq(epsilon))
  }

  def lazyKleene: Frag = {
    val epsilon = Jump()
    val split = Split(epsilon, in)
    val jump = Jump()
    jump.next = split
    setOut(jump)
    Frag(split, Seq(epsilon))
  }

  def greedyPlus: Frag = Frag(dup, greedyKleene)

  def lazyPlus: Frag = Frag(dup, lazyKleene)

  def greedyRange(from: Option[Int], to: Option[Int]): Frag = {
    val required = for (i <- from) yield repeat(i)
    val optional = for (i <- to) yield {
      val n = i - from.getOrElse(0)
      greedyOptional.repeat(n)
    }
    val fragments = required.getOrElse(Nil) ++ optional.getOrElse(Seq(greedyKleene))
    (fragments.head /: fragments.tail) {
      case (lhs, rhs) => Frag(lhs, rhs)
    }
  }

  def lazyRange(from: Option[Int], to: Option[Int]): Frag = {
    val required = for (i <- from) yield repeat(i)
    val optional = for (i <- to) yield {
      val n = i - from.getOrElse(0)
      lazyOptional.repeat(n)
    }
    val fragments = required.getOrElse(Nil) ++ optional.getOrElse(Seq(lazyKleene))
    (fragments.head /: fragments.tail) {
      case (lhs, rhs) => Frag(lhs, rhs)
    }
  }

  def repeatPattern(n: Int): Frag = {
    val fragments = repeat(n)
    (fragments.head /: fragments.tail) {
      case (lhs, rhs) => Frag(lhs, rhs)
    }
  }
}

object Frag {
  def apply(in: Inst, out: Seq[Inst]): Frag = new Frag(in, out)
  def apply(in: Inst): Frag = new Frag(in, Seq(in))
  def apply(f1: Frag, f2: Frag): Frag = {
    f1.setOut(f2.in)
    Frag(f1.in, f2.out)
  }
}


case class Result(start: Int, end: Int, groups: Map[String, Interval])


class Prog(val start: Inst) {
  def findPrefixOf(sent: Int, doc: Document): Option[Result] = findPrefixOf(0, sent, doc)
  def findFirstIn(sent: Int, doc: Document): Option[Result] = findFirstIn(0, sent, doc)
  def findAllIn(sent: Int, doc: Document): Seq[Result] = findAllIn(0, sent, doc)

  def findPrefixOf(tok: Int, sent: Int, doc: Document): Option[Result] = {
    ThompsonVM.evaluate(start, tok, sent, doc) map { m =>
      val (start, end) = m(Prog.GlobalCaptureName)
      Result(start, end, m - Prog.GlobalCaptureName mapValues {
        case (from, until) => Interval(from, until)
      })
    }
  }

  def findFirstIn(tok: Int, sent: Int, doc: Document): Option[Result] = {
    val n = doc.sentences(sent).size
    for (i <- tok until n) {
      val r = findPrefixOf(i, sent, doc)
      if (r.isDefined) return r
    }
    None
  }

  def findAllIn(tok: Int, sent: Int, doc: Document): Seq[Result] = {
    def loop(i: Int): Stream[Result] = findFirstIn(i, sent, doc) match {
      case None => Stream.empty
      case Some(r) => r #:: loop(r.end)
    }
    loop(tok)
  }
}


object Prog {
  val GlobalCaptureName = "--GLOBAL--"

  def compile(input: String): Prog = ProgCompiler.compile(input)
}
