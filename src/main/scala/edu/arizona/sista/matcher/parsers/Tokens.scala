package edu.arizona.sista.matcher

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.{Sentence, Document}



object StringMatcherParser extends RegexParsers {
  def parse(input: String): Prog = parseAll(pattern, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => sys.error(failure.msg)
  }

  def int: Parser[Int] = """\d+""".r ^^ { _.toInt }

  def ident: Parser[String] =
    """\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*""".r

  // single- or double-quote delimited string literal
  def quotedLiteral: Parser[String] =
    """'[^\\']*(?:\\.[^\\']*)*'|"[^\\"]*(?:\\.[^\\"]*)*"""".r ^^ {
      case s => """\\(.)""".r.replaceAllIn(s.drop(1).dropRight(1), m => m.group(1))
    }

  def stringLiteral: Parser[String] = ident | quotedLiteral

  // match a perl style "/" delimited regular expression
  // "\" is the escape character, so "\/" becomes "/"
  def regexLiteral: Parser[Regex] = """/[^\\/]*(?:\\.[^\\/]*)*/""".r ^^ {
    case s => s.drop(1).dropRight(1).replaceAll("""\\/""", "/").r
  }

  def exactStringMatcher: Parser[StringMatcher] = stringLiteral ^^ {
    case string => new ExactStringMatcher(string)
  }

  def regexStringMatcher: Parser[StringMatcher] = regexLiteral ^^ {
    case regex => new RegexStringMatcher(regex)
  }

  def stringMatcher: Parser[StringMatcher] = exactStringMatcher | regexStringMatcher

  def fieldConstraint: Parser[TokenConstraint] = ident ~ "=" ~ stringMatcher ^^ {
    case "word" ~ _ ~ matcher => new WordConstraint(matcher)
    case "lemma" ~ _ ~ matcher => new LemmaConstraint(matcher)
    case "tag" ~ _ ~ matcher => new TagConstraint(matcher)
    case "entity" ~ _ ~ matcher => new EntityConstraint(matcher)
    case "chunk" ~ _ ~ matcher => new ChunkConstraint(matcher)
    case _ => sys.error("unrecognized token field")
  }

  def atomicConstraint: Parser[TokenConstraint] = fieldConstraint | "(" ~> disjunctiveConstraint <~ ")"

  def negatedConstraint: Parser[TokenConstraint] = opt("!") ~ atomicConstraint ^^ {
    case None ~ constraint => constraint
    case Some("!") ~ constraint => new NegatedConstraint(constraint)
  }

  def conjunctiveConstraint: Parser[TokenConstraint] = negatedConstraint ~ rep("&" ~> negatedConstraint) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new ConjunctiveConstraint(lhs, rhs)
    }
  }

  def disjunctiveConstraint: Parser[TokenConstraint] = conjunctiveConstraint ~ rep("|" ~> conjunctiveConstraint) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new DisjunctiveConstraint(lhs, rhs)
    }
  }

  def wordConstraint: Parser[TokenConstraint] = stringMatcher ^^ {
    case matcher => new WordConstraint(matcher)
  }

  def tokenConstraint: Parser[TokenConstraint] = "[" ~> disjunctiveConstraint <~ "]"

  def tokenPattern: Parser[Frag] = (wordConstraint | tokenConstraint) ^^ {
    case constraint => Frag(Match(constraint))
  }

  def capturePattern: Parser[Frag] = "(?<" ~ ident ~ ">" ~ splitPattern ~ ")" ^^ {
    case _ ~ name ~ _ ~ frag ~ _ => capture(name, frag)
  }

  def atomicPattern: Parser[Frag] = tokenPattern | capturePattern

  def repeatedPattern: Parser[Frag] = atomicPattern ~ ("??"|"*?"|"+?"|"?"|"*"|"+") ^^ {
    case pattern ~ "?" => greedyOptional(pattern)
    case pattern ~ "??" => lazyOptional(pattern)
    case pattern ~ "*" => greedyKleene(pattern)
    case pattern ~ "*?" => lazyKleene(pattern)
    case pattern ~ "+" => Frag(pattern, greedyKleene(pattern))
    case pattern ~ "+?" => Frag(pattern, lazyKleene(pattern))
  }

  def rangePattern: Parser[Frag] = atomicPattern ~ "{" ~ int ~ "," ~ int ~ "}" ^^ {
    case frag ~ _ ~ from ~ _ ~ to ~ _ => greedyRange(frag, Some(from), Some(to))
  }

  def fromPattern: Parser[Frag] = atomicPattern ~ "{" ~ int ~ "," ~ "}" ^^ {
    case frag ~ _ ~ from ~ _ ~ _ => greedyRange(frag, Some(from), None)
  }

  def toPattern: Parser[Frag] = atomicPattern ~ "{" ~ "," ~ int ~ "}" ^^ {
    case frag ~ _ ~ _ ~ to ~ _ => greedyRange(frag, None, Some(to))
  }

  def exactPattern: Parser[Frag] = atomicPattern ~ "{" ~ int ~ "}" ^^ {
    case frag ~ _ ~ exact ~ _ => repeatPattern(frag, exact)
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
      val f = capture(Prog.GlobalCaptureName, frag)
      f.setOut(Done)
      new Prog(f.in)
  }

  def capture(name: String, frag: Frag): Frag = {
    val start = SaveStart(name)
    val end = SaveEnd(name)
    start.next = frag.in
    frag.setOut(end)
    Frag(start, Seq(end))
  }

  def greedyOptional(pattern: Frag): Frag = {
    val epsilon = Jump()
    val split = Split(pattern.in, epsilon)
    Frag(split, pattern.out :+ epsilon)
  }

  def lazyOptional(pattern: Frag): Frag = {
    val epsilon = Jump()
    val split = Split(epsilon, pattern.in)
    Frag(split, epsilon +: pattern.out)
  }

  def greedyKleene(pattern: Frag): Frag = {
    val epsilon = Jump()
    val split = Split(pattern.in, epsilon)
    val jump = Jump()
    jump.next = split
    pattern.setOut(jump)
    Frag(split, Seq(epsilon))
  }

  def lazyKleene(pattern: Frag): Frag = {
    val epsilon = Jump()
    val split = Split(epsilon, pattern.in)
    val jump = Jump()
    jump.next = split
    pattern.setOut(jump)
    Frag(split, Seq(epsilon))
  }

  def greedyRange(pattern: Frag, from: Option[Int], to: Option[Int]): Frag = {
    val required = for (i <- from) yield pattern.repeat(i)
    val optional = for (i <- to) yield {
      val n = i - from.getOrElse(0)
      greedyOptional(pattern).repeat(n)
    }
    val fragments = required.getOrElse(Nil) ++ optional.getOrElse(Seq(greedyKleene(pattern)))
    (fragments.head /: fragments.tail) {
      case (lhs, rhs) => Frag(lhs, rhs)
    }
  }

  def repeatPattern(pattern: Frag, n: Int): Frag = {
    val fragments = pattern.repeat(n)
    (fragments.head /: fragments.tail) {
      case (lhs, rhs) => Frag(lhs, rhs)
    }
  }

 }



sealed trait StringMatcher {
  def matches(s: String): Boolean
}

class ExactStringMatcher(string: String) extends StringMatcher {
  def matches(s: String): Boolean = s == string
}

class RegexStringMatcher(regex: Regex) extends StringMatcher {
  def matches(s: String): Boolean = regex.findFirstIn(s).nonEmpty
}



sealed trait Values {
  def word(tok: Int, sent: Int, doc: Document): String = doc.sentences(sent).words(tok)
  def lemma(tok: Int, sent: Int, doc: Document): String = doc.sentences(sent).lemmas.get(tok)
  def tag(tok: Int, sent: Int, doc: Document): String = doc.sentences(sent).tags.get(tok)
  def entity(tok: Int, sent: Int, doc: Document): String = doc.sentences(sent).entities.get(tok)
  def chunk(tok: Int, sent: Int, doc: Document): String = doc.sentences(sent).chunks.get(tok)
}



sealed trait TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document): Boolean
}

class WordConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    matcher.matches(word(tok, sent, doc))
}

class LemmaConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    matcher.matches(lemma(tok, sent, doc))
}

class TagConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    matcher.matches(tag(tok, sent, doc))
}

class EntityConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    matcher.matches(entity(tok, sent, doc))
}

class ChunkConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    matcher.matches(chunk(tok, sent, doc))
}

class NegatedConstraint(constraint: TokenConstraint) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    !constraint.matches(tok, sent, doc)
}

class ConjunctiveConstraint(lhs: TokenConstraint, rhs: TokenConstraint) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    lhs.matches(tok, sent, doc) && rhs.matches(tok, sent, doc)
}

class DisjunctiveConstraint(lhs: TokenConstraint, rhs: TokenConstraint) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    lhs.matches(tok, sent, doc) || rhs.matches(tok, sent, doc)
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
  def find(tok: Int, sent: Int, doc: Document): Option[Result] = {
    ThompsonVM.evaluate(start, tok, sent, doc) map { m =>
      val (start, end) = m(Prog.GlobalCaptureName)
      Result(start, end, m - Prog.GlobalCaptureName mapValues {
        case (from, until) => Interval(from, until)
      })
    }
  }
}


object Prog {
  val GlobalCaptureName = "--GLOBAL--"
}


sealed trait Inst {
  var next: Inst = null
  def dup: Inst
}

case class Split(lhs: Inst, rhs: Inst) extends Inst {
  def dup: Inst = Split(lhs.dup, rhs.dup)
}

case class Match(c: TokenConstraint) extends Inst {
  def dup: Inst = {
    val inst = this.copy()
    if (inst.next != null) inst.next = next.dup
    inst
  }
}

case class Jump() extends Inst {
  def dup: Inst = {
    val inst = this.copy()
    if (inst.next != null) inst.next = next.dup
    inst
  }
}

case class SaveStart(name: String) extends Inst {
  def dup: Inst = {
    val inst = this.copy()
    if (inst.next != null) inst.next = next.dup
    inst
  }
}

case class SaveEnd(name: String) extends Inst {
  def dup: Inst = {
    val inst = this.copy()
    if (inst.next != null) inst.next = next.dup
    inst
  }
}

case object Done extends Inst {
  def dup: Inst = this
}

object ThompsonVM {
  type Sub = Map[String, (Int, Int)]

  private case class Thread(inst: Inst) {
    var sub: Sub = _
  }
  private object Thread {
    def apply(inst: Inst, sub: Sub): Thread = {
      val t = Thread(inst)
      t.sub = sub
      t
    }
  }

  private def mkThreads(inst: Inst, sub: Sub, tok: Int): Seq[Thread] = inst match {
    case i: Jump => mkThreads(i.next, sub, tok)
    case Split(lhs, rhs) => mkThreads(lhs, sub, tok) ++ mkThreads(rhs, sub, tok)
    case i @ SaveStart(name) => mkThreads(i.next, sub + (name -> (tok, -1)), tok)
    case i @ SaveEnd(name) => mkThreads(i.next, sub + (name -> (sub(name)._1, tok)), tok)
    case _ => Seq(Thread(inst, sub))
  }

  def evaluate(start: Inst, tok: Int, sent: Int, doc: Document): Option[Sub] = {

    def nextThreads(threads: Seq[Thread], tok: Int): Seq[Thread] = {
      threads.flatMap(t => t.inst match {
        case i @ Match(c) if c.matches(tok, sent, doc) => mkThreads(i.next, t.sub, tok + 1)
        case _ => Nil
      }).distinct
    }

    def step(tok: Int, threads: Seq[Thread]): (Seq[Thread], Option[Sub]) = {
      val (validThreads, result) = threads.find(_.inst == Done) match {
        case None => (threads, None)
        case Some(t) => (threads.takeWhile(_ != t), Some(t.sub))
      }
      (nextThreads(validThreads, tok), result)
    }

    @tailrec
    def loop(i: Int, threads: Seq[Thread], result: Option[Sub]): Option[Sub] = {
      if (threads.isEmpty) result
      else if (i > doc.sentences(sent).size) result
      else {
        val (ts, res) = step(i, threads)
        loop(i + 1, ts, res)
      }
    }

    loop(tok, mkThreads(start, Map.empty, tok), None)
  }
}
