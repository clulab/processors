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
    val parser1 = rep1sep(concatPattern, ProgramFragment.split)
    val parser2 = withSource("splitPattern", parser1)
    val parser3 = parser2 ^^ { chunks =>
      chunks.tail.foldLeft(chunks.head) {
        case (lhs, rhs) =>
          val split = Split(lhs.in, rhs.in, ProgramFragment.split)
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
  // Note that there is no lazyOptional here.
  def argQuantifier: Parser[String] = {
    import ProgramFragment._

    greedyOptional | lazyStar | greedyStar | lazyPlus | greedyPlus | """\{[0-9,]+\}\??""".r
  }

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

  def isNegative(op: String): Boolean = op.endsWith("!")

  def lookaheadAssertion: Parser[ProgramFragment] = {
    import ProgramFragment._

    val parser1 = "(" ~> (posLookAhead | negLookAhead) ~ splitPattern <~ ")"
    val parser2 = withSource("lookaheadAssertion", parser1)
    val parser3 = parser2 ^^ {
      case op ~ frag =>
        frag.setOut(Done)
        ProgramFragment(MatchLookAhead(frag.in, negative = isNegative(op), reason = op))
    }

    parser3
  }

  // MatchLookBehind builds the pattern in reverse
  def lookbehindAssertion: Parser[ProgramFragment] = {
    import ProgramFragment._

    val parser1 = "(" ~> (posLookBehind | negLookBehind) ~ splitPatternRev <~ ")"
    val parser2 = withSource("lookbehindAssertion", parser1)
    val parser3 = parser2 ^^ {
      case op ~ frag =>
        frag.setOut(Done)
        ProgramFragment(MatchLookBehind(frag.in, negative = isNegative(op), reason = op))
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

  def repetition: Parser[Repetition] =  {
    import ProgramFragment._

    val parser1 = greedyOptional ||| lazyOptional ||| greedyStar ||| lazyStar ||| greedyPlus ||| lazyPlus
    val parser2 = withSource("repetition", parser1)
    val parser3 = parser2 ^^ {
      case repeater: String => new Repetition(repeater)
    }

    parser3
  }

  def repeatedPattern: Parser[ProgramFragment] = {
    val parser1 = atomicPattern ~ repetition
    val parser2 = withSource("repeatedPattern", parser1)
    val parser3 = parser2 ^^ {
      case frag ~ repetition => frag.repetition(repetition)
    }

    parser3
  }

  // positive integer
  def int: Parser[Int] =
    """\d+""".r ^^ { _.toInt }

  def inexactRange: Parser[InexactRange] = {
    val parser1 =  "{" ~ opt(int) ~ "," ~ opt(int) ~ ("}" ||| "}?")
    val parser2 = withSource("inexactRange", parser1)
    val parser3 = parser2 ^^ {
      case "{" ~ fromOpt ~ "," ~ toOpt ~ "}" => new InexactRange(fromOpt, toOpt)
      case "{" ~ fromOpt ~ "," ~ toOpt ~ "}?" => new InexactRange(fromOpt, toOpt, isLazy = true)
      case _ => sys.error("unrecognized rangePattern")
    }

    parser3
  }

  def exactRange: Parser[ExactRange] = {
    val parser1 = "{" ~> int <~ "}"
    val parser2 = withSource("exactRange", parser1)
    val parser3 = parser2 ^^ {
      case n => new ExactRange(n)
    }

    parser3
  }

  def rangePattern: Parser[ProgramFragment] = {
    val parser1 = atomicPattern ~ inexactRange
    val parser2 = withSource("rangePattern", parser1)
    val parser3 = parser2 ^^ {
      case frag ~ inexactRange => frag.inexactRange(inexactRange)
    }

    parser3
  }

  def exactPattern: Parser[ProgramFragment] = {
    val parser1 = atomicPattern ~ exactRange
    val parser2 = withSource("exactPattern", parser1)
    val parser3 = parser2 ^^ {
      case frag ~ exactRange => frag.exactRange(exactRange)
    }

    parser3
  }

  // reverse grammar

  def splitPatternRev: Parser[ProgramFragment] = {
    val parser1 = rep1sep(concatPatternRev, ProgramFragment.split)
    val parser2 = withSource("splitPatternRev", parser1)
    val parser3 = parser2 ^^ { chunks =>
      chunks.tail.foldLeft(chunks.head) {
        case (lhs, rhs) =>
          val split = Split(lhs.in, rhs.in, ProgramFragment.split)
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
    val parser1 = atomicPatternRev ~ repetition
    val parser2 = withSource("repeatedPatternRev", parser1)
    val parser3 = parser2 ^^ {
      case frag ~ repetition => frag.repetition(repetition)
    }

    parser3
  }

  def rangePatternRev: Parser[ProgramFragment] = {
    val parser1 = atomicPatternRev ~ inexactRange
    val parser2 = withSource("rangePatternRev", parser1)
    val parser3 = parser2 ^^ {
      case frag ~ inexactRange => frag.inexactRange(inexactRange)
    }

    parser3
  }

  def exactPatternRev: Parser[ProgramFragment] = {
    val parser1 = atomicPatternRev ~ exactRange
    val parser2 = withSource("exactPatternRev", parser1)
    val parser3 = parser2 ^^ {
      case frag ~ exactRange => frag.exactRange(exactRange)
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
    val reason = ProgramFragment.greedyOptional
    val epsilon = Pass(reason)
    val split = Split(in, epsilon, reason)
    ProgramFragment(split, epsilon :: out)
  }

  def lazyOptional: ProgramFragment = {
    val reason = ProgramFragment.lazyOptional
    val epsilon = Pass(reason)
    val split = Split(epsilon, in, reason)
    ProgramFragment(split, epsilon :: out)
  }

  def greedyStar: ProgramFragment = {
    val reason = ProgramFragment.greedyStar
    val epsilon = Pass(reason)
    val split = Split(in, epsilon, reason)
    setOut(split)
    ProgramFragment(split, epsilon)
  }

  def lazyStar: ProgramFragment = {
    val reason = ProgramFragment.lazyStar
    val epsilon = Pass(reason)
    val split = Split(epsilon, in, reason)
    setOut(split)
    ProgramFragment(split, epsilon)
  }

  def greedyPlus: ProgramFragment = {
    val reason = ProgramFragment.greedyPlus
    val epsilon = Pass(reason)
    val split = Split(in, epsilon, reason)
    setOut(split)
    ProgramFragment(in, epsilon)
  }

  def lazyPlus: ProgramFragment = {
    val reason = ProgramFragment.lazyPlus
    val epsilon = Pass(reason)
    val split = Split(epsilon, in, reason)
    setOut(split)
    ProgramFragment(in, epsilon)
  }

  def inexactRange(inexactRange: InexactRange): ProgramFragment = {
    if (inexactRange.isLazy) lazyRange(inexactRange)
    else greedyRange(inexactRange)
  }

  def exactRange(exactRange: ExactRange): ProgramFragment = {
    repeatPattern(exactRange)
  }

  /** Returns a new ProgramFragment that matches the current fragment
    * between `from` and `to` times greedily.
    */
  def greedyRange(inexactRange: InexactRange): ProgramFragment = {
    val from = inexactRange.fromOpt
    val to = inexactRange.toOpt
    require(from.isDefined || to.isDefined, "either 'from' or 'to' must be specified")
    if (from.isDefined && to.isDefined)
      require(from.get < to.get, "'to' must be greater than 'from'")
    val required = for (i <- from) yield repeat(i)
    val optional = for (i <- to) yield {
      val n = i - from.getOrElse(0)
      greedyOptional.repeat(n)
    }
    val fragments = required.getOrElse(Nil) ++ optional.getOrElse(Seq(greedyStar))
    val withoutSource = ProgramFragment(fragments)
    val withSource = inexactRange.sourceOpt.map { source => withoutSource.copyWithSource(source) }.getOrElse(withoutSource)

    withSource
  }

  /** Returns a new ProgramFragment that matches the current fragment
    * between `from` and `to` times lazily.
    */
  def lazyRange(inexactRange: InexactRange): ProgramFragment = {
    val from = inexactRange.fromOpt
    val to = inexactRange.toOpt
    require(from.isDefined || to.isDefined, "either 'from' or 'to' must be specified")
    if (from.isDefined && to.isDefined)
      require(from.get < to.get, "'to' must be greater than 'from'")
    val required = for (i <- from) yield repeat(i)
    val optional = for (i <- to) yield {
      val n = i - from.getOrElse(0)
      lazyOptional.repeat(n)
    }
    val fragments = required.getOrElse(Nil) ++ optional.getOrElse(Seq(lazyStar))
    val withoutSource = ProgramFragment(fragments)
    val withSource = inexactRange.sourceOpt.map { source => withoutSource.copyWithSource(source) }.getOrElse(withoutSource)

    withSource
  }

  /** Repeats and concatenates the current fragment `n` times. */
  def repeatPattern(exactRange: ExactRange): ProgramFragment = {
    val n = exactRange.at

    ProgramFragment(repeat(n))
  }

  def repetition(repetition: Repetition): ProgramFragment = repetition.repeater match {
    case ProgramFragment.greedyOptional => greedyOptional
    case ProgramFragment.lazyOptional   => lazyOptional
    case ProgramFragment.greedyStar     => greedyStar
    case ProgramFragment.lazyStar       => lazyStar
    case ProgramFragment.greedyPlus     => greedyPlus
    case ProgramFragment.lazyPlus       => lazyPlus
  }
}

class InexactRange(val fromOpt: Option[Int], val toOpt: Option[Int], val isLazy: Boolean = false, val sourceOpt: Option[String] = None) extends Sourced[InexactRange] {

  override def copyWithSource(source: String): InexactRange =
    new InexactRange(fromOpt, toOpt, isLazy, Some(source))
}

class ExactRange(val at: Int, val sourceOpt: Option[String] = None) extends Sourced[ExactRange] {

  override def copyWithSource(source: String): ExactRange =
    new ExactRange(at, Some(source))
}

class Repetition(val repeater: String, val sourceOpt: Option[String] = None) extends Sourced[Repetition] {

  override def copyWithSource(source: String): Repetition =
    new Repetition(repeater, Some(source))
}

object ProgramFragment {
  val greedyOptional = "?"
  val   lazyOptional = "??"
  val     greedyStar = "*"
  val       lazyStar = "*?"
  val     greedyPlus = "+"
  val       lazyPlus = "+?"

  val split = "|"

  val posLookAhead = "?="
  val negLookAhead = "?!"

  val posLookBehind = "?<="
  val negLookBehind = "?<!"

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
          case i @ Split(lhs, rhs, _) => traverse(lhs :: rhs :: rest, seen + i, out)
          case i if i.getNext == null => traverse(rest, seen + i, i :: out)
          case i => traverse(i.getNext :: rest, seen + i, out)
        }
      }
    traverse(List(inst), Set.empty, Nil)
  }

}
