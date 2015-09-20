package edu.arizona.sista.odin.impl

class TokenPatternParsers(val unit: String) extends TokenConstraintParsers {

  // comments are considered whitespace
  override val whiteSpace = """(\s|#.*)+""".r

  def compileTokenPattern(input: String): TokenPattern = parseAll(tokenPattern, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => sys.error(failure.msg)
  }

  def tokenPattern: Parser[TokenPattern] = splitPattern ^^ { frag =>
    val f = frag.capture(TokenPattern.GlobalCapture)
    f.setOut(Done)
    new TokenPattern(f.in)
  }

  def splitPattern: Parser[ProgramFragment] =
    rep1sep(concatPattern, "|") ^^ { chunks =>
      (chunks.head /: chunks.tail) {
        case (lhs, rhs) =>
          val split = Split(lhs.in, rhs.in)
          ProgramFragment(split, lhs.out ++ rhs.out)
      }
    }

  def concatPattern: Parser[ProgramFragment] =
    rep1(quantifiedPattern) ^^ { chunks =>
      (chunks.head /: chunks.tail) {
        case (lhs, rhs) => ProgramFragment(lhs, rhs)
      }
  }

  def quantifiedPattern: Parser[ProgramFragment] =
    atomicPattern ||| repeatedPattern ||| rangePattern ||| exactPattern

  // when matching the default token field (unitConstraint)
  // we need to make sure that the next token is not a ':'
  // only argument names are followed by colon (and a label)
  def singleTokenPattern: Parser[ProgramFragment] =
    (unitConstraint <~ not(":") | tokenConstraint) ^^ {
      case constraint => ProgramFragment(MatchToken(constraint))
    }

  def assertionPattern: Parser[ProgramFragment] =
    sentenceAssertion | lookaheadAssertion | lookbehindAssertion

  def sentenceAssertion: Parser[ProgramFragment] = ("^" | "$") ^^ {
    case "^" => ProgramFragment(MatchSentenceStart())
    case "$" => ProgramFragment(MatchSentenceEnd())
  }

  def lookaheadAssertion: Parser[ProgramFragment] =
    ("(?=" | "(?!") ~ splitPattern <~ ")" ^^ {
      case op ~ frag =>
        frag.setOut(Done)
        val negative = op.endsWith("!")
        ProgramFragment(MatchLookAhead(frag.in, negative))
    }

  // MatchLookBehind requires the length of the pattern's matches,
  // so the pattern is restricted to fixed width patterns only
  def lookbehindAssertion: Parser[ProgramFragment] =
    ("(?<=" | "(?<!") ~ fixedWidthPattern <~ ")" ^^ {
      case op ~ fragWithSize =>
        val (frag, size) = fragWithSize
        frag.setOut(Done)
        val negative = op.endsWith("!")
        ProgramFragment(MatchLookBehind(frag.in, size, negative))
    }

  // a pattern is fixed width if we know the length of its matches at compile time
  // it is a concatenation of fixed width chunks
  def fixedWidthPattern: Parser[(ProgramFragment, Int)] =
    rep1(fixedWidthChunk) ^^ { chunks =>
      (chunks.head /: chunks.tail) {
        case ((lhs, sl), (rhs, sr)) => (ProgramFragment(lhs, rhs), sl + sr)
      }
    }

  // a fixed width atom with optional repetition
  def fixedWidthChunk: Parser[(ProgramFragment, Int)] =
    fixedWidthAtom ~ opt("{" ~> int <~ "}") ^^ {
      case (frag, size) ~ None => (frag, size)
      case (frag, size) ~ Some(n) => (frag.repeatPattern(n), n * size)
    }

  // a fixed width token or a fixed width pattern surrounded by parenthesis
  def fixedWidthAtom: Parser[(ProgramFragment, Int)] =
    fixedWidthToken | "(" ~> fixedWidthPattern <~ ")"

  // a single fixed width token matcher
  def fixedWidthToken: Parser[(ProgramFragment, Int)] =
    singleTokenPattern ^^ { (_, 1) }

  def capturePattern: Parser[ProgramFragment] =
    "(?<" ~ stringLiteral ~ ">" ~ splitPattern ~ ")" ^^ {
      case "(?<" ~ name ~ ">" ~ frag ~ ")" => frag.capture(name)
    }

  def unnamedMentionPattern: Parser[ProgramFragment] =
    "@" ~> exactStringMatcher ^^ {
      case matcher => ProgramFragment(MatchMention(matcher, None))
    }

  def namedMentionPattern: Parser[ProgramFragment] =
    "@" ~> stringLiteral ~ ":" ~ exactStringMatcher ^^ {
      case name ~ ":" ~ matcher => ProgramFragment(MatchMention(matcher, Some(name)))
    }

  def mentionPattern: Parser[ProgramFragment] =
    namedMentionPattern ||| unnamedMentionPattern

  def atomicPattern: Parser[ProgramFragment] =
    assertionPattern | singleTokenPattern | mentionPattern |
    capturePattern | "(" ~> splitPattern <~ ")"

  def repeatedPattern: Parser[ProgramFragment] =
    atomicPattern ~ ("?" ||| "??" ||| "*" ||| "*?" ||| "+" ||| "+?") ^^ {
      case frag ~ "?" => frag.greedyOptional
      case frag ~ "??" => frag.lazyOptional
      case frag ~ "*" => frag.greedyKleene
      case frag ~ "*?" => frag.lazyKleene
      case frag ~ "+" => frag.greedyPlus
      case frag ~ "+?" => frag.lazyPlus
    }

  // positive integer
  def int: Parser[Int] =
    """\d+""".r ^^ { _.toInt }

  def rangePattern: Parser[ProgramFragment] =
    atomicPattern ~ "{" ~ opt(int) ~ "," ~ opt(int) ~ ("}" ||| "}?") ^^ {
      case frag ~ "{" ~ from ~ "," ~ to ~ "}" => frag.greedyRange(from, to)
      case frag ~ "{" ~ from ~ "," ~ to ~ "}?" => frag.lazyRange(from, to)
    }

  def exactPattern: Parser[ProgramFragment] =
    atomicPattern ~ ("{" ~> int <~ "}") ^^ {
      case frag ~ n => frag.repeatPattern(n)
    }

}

/** Represents a partially compiled TokenPattern.
  *
  * Helps the compiler by keeping track of the input and output
  * instructions of a partially compiled TokenPattern.
  */
class ProgramFragment(val in: Inst, val out: Seq[Inst]) {
  import ProgramFragment.findOut

  /** Connects a new instruction to the output instructions.
    *
    * Calling this invalidates the ProgramFragment because
    * the `out` sequence is no longer up to date.
    */
  def setOut(inst: Inst): Unit = out.foreach(_.next = inst)

  private def copy(): ProgramFragment = ProgramFragment(in.deepcopy())

  private def repeat(n: Int): Seq[ProgramFragment] =
    for (i <- 0 until n) yield copy()

  def capture(name: String): ProgramFragment = {
    val start = SaveStart(name)
    val end = SaveEnd(name)
    start.next = in
    setOut(end)
    ProgramFragment(start, end)
  }

  def greedyOptional: ProgramFragment = {
    val epsilon = Jump()
    val split = Split(in, epsilon)
    ProgramFragment(split, epsilon +: out)
  }

  def lazyOptional: ProgramFragment = {
    val epsilon = Jump()
    val split = Split(epsilon, in)
    ProgramFragment(split, epsilon +: out)
  }

  def greedyKleene: ProgramFragment = {
    val epsilon = Jump()
    val split = Split(in, epsilon)
    setOut(split)
    ProgramFragment(split, epsilon)
  }

  def lazyKleene: ProgramFragment = {
    val epsilon = Jump()
    val split = Split(epsilon, in)
    setOut(split)
    ProgramFragment(split, epsilon)
  }

  def greedyPlus: ProgramFragment = {
    val epsilon = Jump()
    val split = Split(in, epsilon)
    setOut(split)
    ProgramFragment(in, epsilon)
  }

  def lazyPlus: ProgramFragment = {
    val epsilon = Jump()
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
    val fragments = required.getOrElse(Nil) ++ optional.getOrElse(Seq(greedyKleene))
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
    val fragments = required.getOrElse(Nil) ++ optional.getOrElse(Seq(lazyKleene))
    ProgramFragment(fragments)
  }

  /** Repeats and concatenates the current fragment `n` times. */
  def repeatPattern(n: Int): ProgramFragment = {
    ProgramFragment(repeat(n))
  }

}

object ProgramFragment {

  def apply(in: Inst, out: Inst): ProgramFragment =
    new ProgramFragment(in, Seq(out))

  def apply(in: Inst, out: Seq[Inst]): ProgramFragment =
    new ProgramFragment(in, out)

  def apply(in: Inst): ProgramFragment =
    new ProgramFragment(in, findOut(in))

  def apply(f1: ProgramFragment, f2: ProgramFragment): ProgramFragment = {
    f1.setOut(f2.in)
    ProgramFragment(f1.in, f2.out)
  }

  def apply(fragments: Seq[ProgramFragment]): ProgramFragment = {
    require(fragments.nonEmpty)
    (fragments.head /: fragments.tail) {
      case (lhs, rhs) => ProgramFragment(lhs, rhs)
    }
  }

  /** Gets an instruction and returns all the output instructions */
  def findOut(inst: Inst): Seq[Inst] = {
    @annotation.tailrec
    def traverse(pending: List[Inst], seen: Set[Inst], out: List[Inst]): Seq[Inst] =
      pending match {
        case Nil => out
        case i :: rest => i match {
        case i if seen contains i => traverse(rest, seen, out)
        case i @ Split(lhs, rhs) => traverse(lhs :: rhs :: rest, seen + i, out)
        case i if i.next == null => traverse(rest, seen + i, i :: out)
        case i => traverse(i.next :: rest, seen + i, out)
        }
      }
    traverse(List(inst), Set.empty, Nil)
  }

}
