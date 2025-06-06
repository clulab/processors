package org.clulab.odin.impl

import scala.language.reflectiveCalls

class TokenPatternParsers(val unit: String, val config: OdinConfig) extends TokenConstraintParsers {

  // comments are considered whitespace
  override val whiteSpace = """(\s|#.*)+""".r

  def compileTokenPattern(input: String): TokenPattern = parseAll(tokenPattern, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => sys.error(failure.msg)
  }

  def tokenPattern: Parser[TokenPattern] = {
    val parser1 = splitPattern
    val parser2 = withSource("tokenPattern", parser1)
    val parser3 = parser2 ^^ { frag =>
      val f = frag.capture(TokenPattern.GlobalCapture)
      f.setOut(Done)
      new TokenPattern(f.in)
    }

    parser3
  }

  def splitPattern: Parser[ProgramFragment] = {
    val parser1 = rep1sep(concatPattern, "|")
    val parser2 = withSource("splitPattern", parser1)
    val parser3 = parser2 ^^ { chunks =>
      chunks.tail.foldLeft(chunks.head) {
        case (lhs, rhs) =>
          val split = Split(lhs.in, rhs.in)
          ProgramFragment(split, lhs.out ++ rhs.out)
      }
    }

    parser3
  }

  def concatPattern: Parser[ProgramFragment] = {
    val parser1 = rep1(quantifiedPattern)
    val parser2 = withSource("concatPattern", parser1)
    val parser3 = parser2 ^^ { chunks =>
      chunks.tail.foldLeft(chunks.head) {
        case (lhs, rhs) => ProgramFragment(lhs, rhs)
      }
    }

    parser3
  }

  def quantifiedPattern: Parser[ProgramFragment] =
      (atomicPattern ||| repeatedPattern ||| rangePattern ||| exactPattern)

  // In GraphPatterns each argument can have a quantifier.
  // This parser accepts something that looks like an arg quantifier.
  // Used in singleTokenPattern, as part of a negative lookahead
  def argQuantifier: Parser[String] =
    "?" | "*?" | "*" | "+?" | "+" | """\{[0-9,]+\}\??""".r

  // when matching the default token field (unitConstraint)
  // we need to make sure that the next token is not a ':'
  // only argument names are followed by colon (and a label)
  // OR
  // in case the arg was written as name:label (which looks like an odinIdentifier)
  // we need to ensure it is not followed by '=' (with an optional argument quantifier in between)
  def singleTokenPattern: Parser[ProgramFragment] = {
    val parser1 = unitConstraint <~ not(":" | opt(argQuantifier) ~ "=") | tokenConstraint
    val parser2 = withSource("singleTokenPattern", parser1)
    val parser3 = parser2 ^^ {
      case constraint => ProgramFragment(MatchToken(constraint))
    }

    parser3
  }

  def assertionPattern: Parser[ProgramFragment] =
      sentenceAssertion | lookaheadAssertion | lookbehindAssertion

  def sentenceAssertion: Parser[ProgramFragment] = {
    val parser1 = "^" | "$"
    val parser2 = withSource("sentenceAssertion", parser1)
    val parser3 = parser2 ^^ {
      case "^" => ProgramFragment(MatchSentenceStart())
      case "$" => ProgramFragment(MatchSentenceEnd())
    }

    parser3
  }

  def lookaheadAssertion: Parser[ProgramFragment] = {
    val parser1 = ("(?=" | "(?!") ~ splitPattern <~ ")"
    val parser2 = withSource("lookaheadAssertion", parser1)
    val parser3 = parser2 ^^ {
      case op ~ frag =>
        frag.setOut(Done)
        ProgramFragment(MatchLookAhead(frag.in, op.endsWith("!")))
    }

    parser3
  }

  // MatchLookBehind builds the pattern in reverse
  def lookbehindAssertion: Parser[ProgramFragment] = {
    val parser1 = ("(?<=" | "(?<!") ~ splitPatternRev <~ ")"
    val parser2 = withSource("lookbehindAssertion", parser1)
    val parser3 = parser2 ^^ {
      case op ~ frag =>
        frag.setOut(Done)
        ProgramFragment(MatchLookBehind(frag.in, op.endsWith("!")))
    }

    parser3
  }

  def capturePattern: Parser[ProgramFragment] = {
    val parser1 = "(?<" ~ javaIdentifier ~ ">" ~ splitPattern ~ ")"
    val parser2 = withSource("capturePattern", parser1)
    val parser3 = parser2 ^^ {
      case "(?<" ~ name ~ ">" ~ frag ~ ")" => frag.capture(name)
      case _ => sys.error("unrecognized capturePattern")
    }

    parser3
  }

  def mentionPattern: Parser[ProgramFragment] = {
    val parser1 = "@" ~> opt(javaIdentifier <~ ":") ~ exactStringMatcher ~ opt("." ~> javaIdentifier)
    val parser2 = withSource("mentionPattern", parser1)
    val parser3 = parser2 ^^ {
      case name ~ matcher ~ arg => ProgramFragment(MatchMention(matcher, name, arg))
    }

    parser3
  }

  def atomicPattern: Parser[ProgramFragment] =
    assertionPattern | singleTokenPattern | mentionPattern |
    capturePattern | "(" ~> splitPattern <~ ")"

  def repeatedPattern: Parser[ProgramFragment] = {
    val parser1 = atomicPattern ~ ("?" ||| "??" ||| "*" ||| "*?" ||| "+" ||| "+?")
    val parser2 = withSource("repeatedPattern", parser1)
    val parser3 = parser2 ^^ {
      case frag ~ "?"  => frag.greedyOptional
      case frag ~ "??" => frag.lazyOptional
      case frag ~ "*"  => frag.greedyStar
      case frag ~ "*?" => frag.lazyStar
      case frag ~ "+"  => frag.greedyPlus
      case frag ~ "+?" => frag.lazyPlus
      case _ => sys.error("unrecognized repeatedPattern operator")
    }

    parser3
  }

  // positive integer
  def int: Parser[Int] =
    """\d+""".r ^^ { _.toInt }

  def rangePattern: Parser[ProgramFragment] = {
    val parser1 = atomicPattern ~ "{" ~ opt(int) ~ "," ~ opt(int) ~ ("}" ||| "}?")
    val parser2 = withSource("rangePattern", parser1)
    val parser3 = parser2 ^^ {
      case frag ~ "{" ~ from ~ "," ~ to ~ "}" => frag.greedyRange(from, to)
      case frag ~ "{" ~ from ~ "," ~ to ~ "}?" => frag.lazyRange(from, to)
      case _ => sys.error("unrecognized rangePattern")
    }

    parser3
  }

  def exactPattern: Parser[ProgramFragment] = {
    val parser1 = atomicPattern ~ ("{" ~> int <~ "}")
    val parser2 = withSource("exactPattern", parser1)
    val parser3 = parser2 ^^ {
      case frag ~ n => frag.repeatPattern(n)
    }

    parser3
  }

  // reverse grammar

  def splitPatternRev: Parser[ProgramFragment] = {
    val parser1 = rep1sep(concatPatternRev, "|")
    val parser2 = withSource("splitPatternRev", parser1)
    val parser3 = parser2 ^^ { chunks =>
      chunks.tail.foldLeft(chunks.head) {
        case (lhs, rhs) =>
          val split = Split(lhs.in, rhs.in)
          ProgramFragment(split, lhs.out ++ rhs.out)
      }
    }

    parser3
  }

  def concatPatternRev: Parser[ProgramFragment] = {
    val parser1 = rep1(quantifiedPatternRev)
    val parser2 = withSource("concatPatternRev", parser1)
    val parser3 = parser2 ^^ { chunks =>
      chunks.tail.foldLeft(chunks.head) {
        case (lhs, rhs) => ProgramFragment(rhs, lhs)
      }
    }

    parser3
  }

  def quantifiedPatternRev: Parser[ProgramFragment] =
    atomicPatternRev ||| repeatedPatternRev ||| rangePatternRev ||| exactPatternRev

  def capturePatternRev: Parser[ProgramFragment] = {
    val parser1 = "(?<" ~ javaIdentifier ~ ">" ~ splitPatternRev ~ ")"
    val parser2 = withSource("capturePatternRev", parser1)
    val parser3 = parser2 ^^ {
      case "(?<" ~ name ~ ">" ~ frag ~ ")" => frag.capture(name)
      case _ => sys.error("unrecognized capturePatternRev")
    }

    parser3
  }

  def atomicPatternRev: Parser[ProgramFragment] =
    assertionPattern | singleTokenPattern | mentionPattern |
    capturePatternRev | "(" ~> splitPatternRev <~ ")"

  def repeatedPatternRev: Parser[ProgramFragment] = {
    val parser1 = atomicPatternRev ~ ("?" ||| "??" ||| "*" ||| "*?" ||| "+" ||| "+?")
    val parser2 = withSource("repeatedPatternRev", parser1)
    val parser3 = parser2 ^^ {
      case frag ~ "?" => frag.greedyOptional
      case frag ~ "??" => frag.lazyOptional
      case frag ~ "*" => frag.greedyStar
      case frag ~ "*?" => frag.lazyStar
      case frag ~ "+" => frag.greedyPlus
      case frag ~ "+?" => frag.lazyPlus
      case _ => sys.error("unrecognized repeatedPatternRev operator")
    }

    parser3
  }

  def rangePatternRev: Parser[ProgramFragment] = {
    val parser1 = atomicPatternRev ~ "{" ~ opt(int) ~ "," ~ opt(int) ~ ("}" ||| "}?")
    val parser2 = withSource("rangePatternRev", parser1)
    val parser3 = parser2 ^^ {
      case frag ~ "{" ~ from ~ "," ~ to ~ "}" => frag.greedyRange(from, to)
      case frag ~ "{" ~ from ~ "," ~ to ~ "}?" => frag.lazyRange(from, to)
      case _ => sys.error("unrecognized rangePatternRev")
    }

    parser3
  }

  def exactPatternRev: Parser[ProgramFragment] = {
    val parser1 = atomicPatternRev ~ ("{" ~> int <~ "}")
    val parser2 = withSource("exactPatternRev", parser1)
    val parser3 = parser2 ^^ {
      case frag ~ n => frag.repeatPattern(n)
    }

    parser3
  }

}

/** Represents a partially compiled TokenPattern.
  *
  * Helps the compiler by keeping track of the input and output
  * instructions of a partially compiled TokenPattern.
  */
class ProgramFragment(val in: Inst, val out: List[Inst], override val sourceOpt: Option[String] = None) extends Sourced[ProgramFragment] {

  override def copyWithSource(source: String): ProgramFragment = {
    new ProgramFragment(in, out, Some(source))
  }

  import ProgramFragment.findOut

  /** Connects a new instruction to the output instructions.
    *
    * Calling this invalidates the ProgramFragment because
    * the `out` sequence is no longer up to date.
    */
  def setOut(inst: Inst): Unit = out.foreach(_.setNext(inst))

  private def copy(): ProgramFragment = ProgramFragment(in.deepcopy())

  private def repeat(n: Int): Seq[ProgramFragment] =
    for (i <- 0 until n) yield copy()

  def capture(name: String): ProgramFragment = {
    val start = SaveStart(name, in)
    val end = SaveEnd(name)

    setOut(end)
    ProgramFragment(start, end)
  }

  def greedyOptional: ProgramFragment = {
    val epsilon = Pass()
    val split = Split(in, epsilon)
    ProgramFragment(split, epsilon :: out)
  }

  def lazyOptional: ProgramFragment = {
    val epsilon = Pass()
    val split = Split(epsilon, in)
    ProgramFragment(split, epsilon :: out)
  }

  def greedyStar: ProgramFragment = {
    val epsilon = Pass()
    val split = Split(in, epsilon)
    setOut(split)
    ProgramFragment(split, epsilon)
  }

  def lazyStar: ProgramFragment = {
    val epsilon = Pass()
    val split = Split(epsilon, in)
    setOut(split)
    ProgramFragment(split, epsilon)
  }

  def greedyPlus: ProgramFragment = {
    val epsilon = Pass()
    val split = Split(in, epsilon)
    setOut(split)
    ProgramFragment(in, epsilon)
  }

  def lazyPlus: ProgramFragment = {
    val epsilon = Pass()
    val split = Split(epsilon, in)
    setOut(split)
    ProgramFragment(in, epsilon)
  }

  /** Returns a new ProgramFragment that matches the current fragment
    * between `from` and `to` times greedily.
    */
  def greedyRange(from: Option[Int], to: Option[Int]): ProgramFragment = {
    require(from.isDefined || to.isDefined, "either 'from' or 'to' must be specified")
    if (from.isDefined && to.isDefined)
      require(from.get < to.get, "'to' must be greater than 'from'")
    val required = for (i <- from) yield repeat(i)
    val optional = for (i <- to) yield {
      val n = i - from.getOrElse(0)
      greedyOptional.repeat(n)
    }
    val fragments = required.getOrElse(Nil) ++ optional.getOrElse(Seq(greedyStar))
    ProgramFragment(fragments)
  }

  /** Returns a new ProgramFragment that matches the current fragment
    * between `from` and `to` times lazily.
    */
  def lazyRange(from: Option[Int], to: Option[Int]): ProgramFragment = {
    require(from.isDefined || to.isDefined, "either 'from' or 'to' must be specified")
    if (from.isDefined && to.isDefined)
      require(from.get < to.get, "'to' must be greater than 'from'")
    val required = for (i <- from) yield repeat(i)
    val optional = for (i <- to) yield {
      val n = i - from.getOrElse(0)
      lazyOptional.repeat(n)
    }
    val fragments = required.getOrElse(Nil) ++ optional.getOrElse(Seq(lazyStar))
    ProgramFragment(fragments)
  }

  /** Repeats and concatenates the current fragment `n` times. */
  def repeatPattern(n: Int): ProgramFragment = {
    ProgramFragment(repeat(n))
  }
}

object ProgramFragment {

  def apply(in: Inst, out: Inst): ProgramFragment =
    new ProgramFragment(in, List(out))

  def apply(in: Inst, out: Seq[Inst]): ProgramFragment =
    new ProgramFragment(in, out.toList)

  def apply(in: Inst): ProgramFragment =
    new ProgramFragment(in, findOut(in))

  def apply(f1: ProgramFragment, f2: ProgramFragment): ProgramFragment = {
    f1.setOut(f2.in)
    new ProgramFragment(f1.in, f2.out)
  }

  def apply(fragments: Seq[ProgramFragment]): ProgramFragment = {
    require(fragments.nonEmpty)
    fragments.tail.foldLeft(fragments.head) {
      case (lhs, rhs) => ProgramFragment(lhs, rhs)
    }
  }

  /** Gets an instruction and returns all the output instructions */
  def findOut(inst: Inst): List[Inst] = {
    @annotation.tailrec
    def traverse(pending: List[Inst], seen: Set[Inst], out: List[Inst]): List[Inst] =
      pending match {
        case Nil => out
        case i :: rest => i match {
          case i if seen contains i => traverse(rest, seen, out)
          case i @ Split(lhs, rhs) => traverse(lhs :: rhs :: rest, seen + i, out)
          case i if i.getNext == null => traverse(rest, seen + i, i :: out)
          case i => traverse(i.getNext :: rest, seen + i, out)
        }
      }
    traverse(List(inst), Set.empty, Nil)
  }

}
