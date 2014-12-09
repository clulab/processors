package edu.arizona.sista.matcher

trait TokenPatternParsers extends TokenConstraintParsers {
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
    repeatedPattern | rangePattern | fromPattern | toPattern | exactPattern | atomicPattern

  def singleTokenPattern: Parser[ProgramFragment] = (wordConstraint | tokenConstraint) ^^ {
    case constraint => ProgramFragment(Match(constraint))
  }

  def capturePattern: Parser[ProgramFragment] = "(?<" ~ ident ~ ">" ~ splitPattern ~ ")" ^^ {
    case _ ~ name ~ _ ~ frag ~ _ => frag.capture(name)
  }

  def atomicPattern: Parser[ProgramFragment] = singleTokenPattern | capturePattern | "(" ~> splitPattern <~ ")"

  def repeatedPattern: Parser[ProgramFragment] = atomicPattern ~ ("??"|"*?"|"+?"|"?"|"*"|"+") ^^ {
    case frag ~ "?" => frag.greedyOptional
    case frag ~ "??" => frag.lazyOptional
    case frag ~ "*" => frag.greedyKleene
    case frag ~ "*?" => frag.lazyKleene
    case frag ~ "+" => frag.greedyPlus
    case frag ~ "+?" => frag.lazyPlus
  }

  def rangePattern: Parser[ProgramFragment] = atomicPattern ~ "{" ~ int ~ "," ~ int ~ ("}?"|"}") ^^ {
    case frag ~ _ ~ from ~ _ ~ to ~ "}" => frag.greedyRange(Some(from), Some(to))
    case frag ~ _ ~ from ~ _ ~ to ~ "}?" => frag.lazyRange(Some(from), Some(to))
  }

  def fromPattern: Parser[ProgramFragment] = atomicPattern ~ "{" ~ int ~ "," ~ ("}?"|"}") ^^ {
    case frag ~ _ ~ from ~ _ ~ "}" => frag.greedyRange(Some(from), None)
    case frag ~ _ ~ from ~ _ ~ "}?" => frag.lazyRange(Some(from), None)
  }

  def toPattern: Parser[ProgramFragment] = atomicPattern ~ "{" ~ "," ~ int ~ ("}?"|"}") ^^ {
    case frag ~ _ ~ _ ~ to ~ "}" => frag.greedyRange(None, Some(to))
    case frag ~ _ ~ _ ~ to ~ "}?" => frag.lazyRange(None, Some(to))
  }

  def exactPattern: Parser[ProgramFragment] = atomicPattern ~ "{" ~ int ~ "}" ^^ {
    case frag ~ _ ~ n ~ _ => frag.repeatPattern(n)
  }

  // this class is only used while compiling a token pattern
  class ProgramFragment(val in: Inst, val out: Seq[Inst]) {
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
    def apply(in: Inst): ProgramFragment = new ProgramFragment(in, Seq(in))
    def apply(f1: ProgramFragment, f2: ProgramFragment): ProgramFragment = {
      f1.setOut(f2.in)
      ProgramFragment(f1.in, f2.out)
    }
  }
}
