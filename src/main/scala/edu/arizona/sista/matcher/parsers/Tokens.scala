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

  def repeatedPattern: Parser[Frag] = atomicPattern ~ opt("??"|"*?"|"+?"|"?"|"*"|"+") ^^ {
    case pattern ~ None => pattern
    case pattern ~ Some("?") => greedyOptional(pattern)
    case pattern ~ Some("??") => lazyOptional(pattern)
    case pattern ~ Some("*") => greedyKleene(pattern)
    case pattern ~ Some("*?") => lazyKleene(pattern)
    case pattern ~ Some("+") => Frag(pattern, greedyKleene(pattern))
    case pattern ~ Some("+?") => Frag(pattern, lazyKleene(pattern))
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

  def quantifiedPattern: Parser[Frag] = repeatedPattern | rangePattern | fromPattern | toPattern | exactPattern

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
      val name = "--GLOBAL--"
      val f = capture(name, frag)
      f.setOut(Done)
      new Prog(f.in, -1)
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
    val required = for (i <- from) yield Seq.fill(i)(pattern)
    val optional = for (i <- to) yield {
      val opt = i - from.getOrElse(0)
      Seq.fill(opt)(greedyOptional(pattern))
    }
    val fragments = required.getOrElse(Nil) ++ optional.getOrElse(Seq(greedyKleene(pattern)))
    (fragments.head /: fragments.tail) {
      case (lhs, rhs) => Frag(lhs, rhs)
    }
  }

  def repeatPattern(pattern: Frag, n: Int): Frag = {
    val fragments = Seq.fill(n)(pattern)
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
}

object Frag {
  def apply(in: Inst, out: Seq[Inst]): Frag = new Frag(in, out)
  def apply(in: Inst): Frag = new Frag(in, Seq(in))
  def apply(f1: Frag, f2: Frag): Frag = {
    f1.setOut(f2.in)
    Frag(f1.in, f2.out)
  }
}



class Prog(val start: Inst, val len: Int)

sealed trait Inst {
  var next: Inst = _
}

case class Match(c: TokenConstraint) extends Inst
case class Split(var lhs: Inst, var rhs: Inst) extends Inst
case class Jump() extends Inst
case class SaveStart(name: String) extends Inst
case class SaveEnd(name: String) extends Inst
case object Done extends Inst

object ThompsonVM {
  type Sub = Map[String, Interval]

  // sub shouldn't count for Thread.equals
  private case class Thread(inst: Inst, sub: Sub)

  private def mkThreads(inst: Inst, sub: Sub, tok: Int): Seq[Thread] = inst match {
    case i: Jump => mkThreads(i.next, sub, tok)
    case Split(lhs, rhs) => mkThreads(lhs, sub, tok) ++ mkThreads(rhs, sub, tok)
    case i @ SaveStart(name) => mkThreads(i.next, sub + (name -> Interval(tok)), tok)
    case i @ SaveEnd(name) => mkThreads(i.next, sub + (name -> Interval(sub(name).start, tok)), tok)
    case _ => Seq(Thread(inst, sub))
  }

  def execute(prog: Prog, tok: Int, sent: Int, doc: Document): Option[Sub] = {
    def step(tok: Int, threads: Seq[Thread]): Either[Seq[Thread], Sub] =
    Left(threads.flatMap(t => t.inst match {
      case Done => return Right(t.sub)
      case i @ Match(c) if c.matches(tok, sent, doc) => mkThreads(i.next, t.sub, tok)
      case _ => Nil
    }).distinct)

    @tailrec
    def loop(i: Int, threads: Seq[Thread]): Option[Sub] = {
      if (threads.isEmpty) None
      else step(i, threads) match {
        case Left(ts) => loop(i + 1, ts)
        case Right(sub) => Some(sub)
      }
    }

    loop(tok, mkThreads(prog.start, Map.empty, tok))
  }
}
