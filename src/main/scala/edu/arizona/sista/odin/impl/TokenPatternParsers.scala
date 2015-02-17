package edu.arizona.sista.odin.impl

trait TokenPatternParsers extends TokenConstraintParsers {
  // comments are considered whitespace
  override val whiteSpace = """(\s|#.*)+""".r

  def tokenPattern: Parser[TokenPattern] = splitPattern ^^ {
    case frag =>
      val f = frag.capture(TokenPattern.GlobalCapture)
      f.setOut(Done)
      new TokenPattern(f.in)
  }

  def splitPattern: Parser[ProgramFragment] = concatPattern ~ rep("|" ~> concatPattern) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) =>
        val split = Split(lhs.in, rhs.in)
        ProgramFragment(split, lhs.out ++ rhs.out)
    }
  }

  def concatPattern: Parser[ProgramFragment] = quantifiedPattern ~ rep(quantifiedPattern) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => ProgramFragment(lhs, rhs)
    }
  }

  def quantifiedPattern: Parser[ProgramFragment] =
    repeatedPattern | rangePattern | exactPattern | atomicPattern

  def singleTokenPattern: Parser[ProgramFragment] = (wordConstraint | tokenConstraint) ^^ {
    case constraint => ProgramFragment(MatchToken(constraint))
  }

  def zeroWidthAssertion: Parser[ProgramFragment] = ("^"|"$") ^^ {
    case "^" => ProgramFragment(MatchSentenceStart())
    case "$" => ProgramFragment(MatchSentenceEnd())
  }

  def capturePattern: Parser[ProgramFragment] = "(?<" ~ ident ~ ">" ~ splitPattern ~ ")" ^^ {
    case _ ~ name ~ _ ~ frag ~ _ => frag.capture(name)
  }

  def mentionPattern: Parser[ProgramFragment] = "@" ~> exactStringMatcher ^^ {
    case matcher => ProgramFragment(MatchMention(matcher))
  }

  def atomicPattern: Parser[ProgramFragment] =
    zeroWidthAssertion | singleTokenPattern | mentionPattern | capturePattern | "(" ~> splitPattern <~ ")"

  def repeatedPattern: Parser[ProgramFragment] = atomicPattern ~ ("??"|"*?"|"+?"|"?"|"*"|"+") ^^ {
    case frag ~ "?" => frag.greedyOptional
    case frag ~ "??" => frag.lazyOptional
    case frag ~ "*" => frag.greedyKleene
    case frag ~ "*?" => frag.lazyKleene
    case frag ~ "+" => frag.greedyPlus
    case frag ~ "+?" => frag.lazyPlus
  }

  def rangePattern: Parser[ProgramFragment] = atomicPattern ~ "{" ~ opt(int) ~ "," ~ opt(int) ~ ("}?"|"}") ^^ {
    case frag ~ _ ~ from ~ _ ~ to ~ "}" => frag.greedyRange(from, to)
    case frag ~ _ ~ from ~ _ ~ to ~ "}?" => frag.lazyRange(from, to)
  }

  def exactPattern: Parser[ProgramFragment] = atomicPattern ~ "{" ~ int ~ "}" ^^ {
    case frag ~ _ ~ n ~ _ => frag.repeatPattern(n)
  }

  // this class is only used while compiling a token pattern
  class ProgramFragment(val in: Inst, val out: Seq[Inst]) {
    import ProgramFragment.findOut

    def setOut(inst: Inst): Unit = out.foreach(_.next = inst)

    def dup: ProgramFragment = {
      val newIn = in.dup
      ProgramFragment(newIn, findOut(newIn))
    }

    def repeat(n: Int): Seq[ProgramFragment] = {
      for (i <- 0 until n) yield dup
    }

    def capture(name: String): ProgramFragment = {
      val start = SaveStart(name)
      val end = SaveEnd(name)
      start.next = in
      setOut(end)
      ProgramFragment(start, Seq(end))
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
      val jump = Jump()
      jump.next = split
      setOut(jump)
      ProgramFragment(split, Seq(epsilon))
    }

    def lazyKleene: ProgramFragment = {
      val epsilon = Jump()
      val split = Split(epsilon, in)
      val jump = Jump()
      jump.next = split
      setOut(jump)
      ProgramFragment(split, Seq(epsilon))
    }

    def greedyPlus: ProgramFragment = ProgramFragment(dup, greedyKleene)

    def lazyPlus: ProgramFragment = ProgramFragment(dup, lazyKleene)

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
      (fragments.head /: fragments.tail) {
        case (lhs, rhs) => ProgramFragment(lhs, rhs)
      }
    }

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
      (fragments.head /: fragments.tail) {
        case (lhs, rhs) => ProgramFragment(lhs, rhs)
      }
    }

    def repeatPattern(n: Int): ProgramFragment = {
      val fragments = repeat(n)
      (fragments.head /: fragments.tail) {
        case (lhs, rhs) => ProgramFragment(lhs, rhs)
      }
    }
  }

  object ProgramFragment {
    def apply(in: Inst, out: Seq[Inst]): ProgramFragment = new ProgramFragment(in, out)
    def apply(in: Inst): ProgramFragment = new ProgramFragment(in, findOut(in))
    def apply(f1: ProgramFragment, f2: ProgramFragment): ProgramFragment = {
      f1.setOut(f2.in)
      ProgramFragment(f1.in, f2.out)
    }

    def findOut(i: Inst): Seq[Inst] = i match {
      case Split(lhs, rhs) => findOut(lhs) ++ findOut(rhs)
      case i if i.next == null => Seq(i)
      case i => findOut(i.next)
    }
  }
}
