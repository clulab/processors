package edu.arizona.sista.odin.impl

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin._

trait DependencyPatternParsers extends TokenPatternParsers {
  // comments are considered whitespace
  override val whiteSpace = """([ \t\x0B\f\r]|#.*)+""".r
  val eol = "\n"

  def dependencyPattern: Parser[DependencyPattern] =
    triggerFinder ~ rep1(eol) ~ repsep(argPattern, rep1(eol)) ^^ {
      case trigger ~ _ ~ arguments => new DependencyPattern(trigger, arguments)
    }

  def triggerFinder: Parser[TokenPattern] = "(?i)trigger".r ~> "=" ~> tokenPattern

  def argPattern: Parser[ArgumentPattern] =
    ident ~ ":" ~ ident ~ opt("?") ~ "=" ~ disjunctiveDepPattern ^^ {
      case name ~ _ ~ _ ~ _ ~ _ ~ _ if name.compareToIgnoreCase("trigger") == 0 =>
        sys.error("`trigger` is not a valid argument name")
      case name ~ ":" ~ label ~ None ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, true)
      case name ~ ":" ~ label ~ Some("?") ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, false)
    }

  def disjunctiveDepPattern: Parser[DependencyPatternNode] =
    concatDepPattern ~ rep("|" ~> concatDepPattern) ^^ {
      case first ~ rest => (first /: rest) {
        case (lhs, rhs) => new DisjunctiveDependencyPattern(lhs, rhs)
      }
    }

  def concatDepPattern: Parser[DependencyPatternNode] =
    filteredDepPattern ~ rep(filteredDepPattern) ^^ {
      case first ~ rest => (first /: rest) {
        case (lhs, rhs) => new ConcatDependencyPattern(lhs, rhs)
      }
    }

  def filterableDepPattern: Parser[DependencyPatternNode] =
    repeatDepPattern | rangeDepPattern | quantifiedDepPattern | atomicDepPattern

  def filteredDepPattern: Parser[DependencyPatternNode] =
    filterableDepPattern ~ opt(tokenConstraint) ^^ {
      case pat ~ None => pat
      case pat ~ Some(constraint) => new FilteredDependencyPattern(pat, constraint)
    }

  def quantifiedDepPattern: Parser[DependencyPatternNode] =
    atomicDepPattern ~ ("?"|"*"|"+") ^^ {
      case pat ~ "?" => new OptionalDependencyPattern(pat)
      case pat ~ "*" => new KleeneDependencyPattern(pat)
      case pat ~ "+" => new ConcatDependencyPattern(pat, new KleeneDependencyPattern(pat))
    }

  // helper function that repeats a pattern N times
  private def repeatPattern(pattern: DependencyPatternNode, n: Int): DependencyPatternNode = {
    require(n > 0, "'n' must be greater than zero")
    (pattern /: Seq.fill(n - 1)(pattern)) {
      case (lhs, rhs) => new ConcatDependencyPattern(lhs, rhs)
    }
  }

  def repeatDepPattern: Parser[DependencyPatternNode] =
    atomicDepPattern ~ "{" ~ int ~ "}" ^^ {
      case pat ~ "{" ~ n ~ "}" => repeatPattern(pat, n)
    }

  def rangeDepPattern: Parser[DependencyPatternNode] =
    atomicDepPattern ~ "{" ~ opt(int) ~ "," ~ opt(int) ~ "}" ^^ {
      case pat ~ "{" ~ from ~ "," ~ to ~ "}" => (from, to) match {
        case (None, None) =>
          sys.error("invalid range")
        case (None, Some(n)) =>
          repeatPattern(new OptionalDependencyPattern(pat), n)
        case (Some(m), None) =>
          val req = repeatPattern(pat, m)
          val kleene = new KleeneDependencyPattern(pat)
          new ConcatDependencyPattern(req, kleene)
        case (Some(m), Some(n)) =>
          require(n > m, "'to' must be greater than 'from'")
          val req = repeatPattern(pat, m)
          val opt = repeatPattern(new OptionalDependencyPattern(pat), n - m)
          new ConcatDependencyPattern(req, opt)
      }
    }

  def atomicDepPattern: Parser[DependencyPatternNode] =
    outgoingDepPattern | incomingDepPattern | "(" ~> disjunctiveDepPattern <~ ")"

  def outgoingDepPattern: Parser[DependencyPatternNode] =
    opt(">") ~> stringMatcher ^^ { new OutgoingDependencyPattern(_) }

  def incomingDepPattern: Parser[DependencyPatternNode] =
    "<" ~> stringMatcher ^^ { new IncomingDependencyPattern(_) }
}

class ArgumentPattern(val name: String, val label: String, pattern: DependencyPatternNode, val required: Boolean) {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: Option[State]): Seq[Interval] =
    pattern.findAllIn(tok, sent, doc, state) map Interval.apply
}

sealed trait DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: Option[State]): Seq[Int]
}

class OutgoingDependencyPattern(matcher: StringMatcher)
extends DependencyPatternNode with Dependencies {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: Option[State]): Seq[Int] = {
    val edges = outgoingEdges(sent, doc)
    if (edges isDefinedAt tok) matcher.filter(edges(tok)) else Nil
  }
}

class IncomingDependencyPattern(matcher: StringMatcher)
extends DependencyPatternNode with Dependencies {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: Option[State]): Seq[Int] = {
    val edges = incomingEdges(sent, doc)
    if (edges isDefinedAt tok) matcher.filter(edges(tok)) else Nil
  }
}

class ConcatDependencyPattern(lhs: DependencyPatternNode, rhs: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: Option[State]): Seq[Int] =
    (lhs.findAllIn(tok, sent, doc, state) flatMap (i => rhs.findAllIn(i, sent, doc, state))).distinct
}

class DisjunctiveDependencyPattern(lhs: DependencyPatternNode, rhs: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: Option[State]): Seq[Int] =
    (lhs.findAllIn(tok, sent, doc, state) ++ rhs.findAllIn(tok, sent, doc, state)).distinct
}

class FilteredDependencyPattern(pattern: DependencyPatternNode, constraint: TokenConstraint)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: Option[State]): Seq[Int] = {
    val tokens = pattern.findAllIn(tok, sent, doc, state)
    constraint.filter(tokens, sent, doc, state)
  }
}

class OptionalDependencyPattern(pattern: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: Option[State]): Seq[Int] =
    (tok +: pattern.findAllIn(tok, sent, doc, state)).distinct
}

class KleeneDependencyPattern(pattern: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: Option[State]): Seq[Int] = {
    @annotation.tailrec
    def loop(remaining: Seq[Int], results: Seq[Int]): Seq[Int] = remaining match {
      case Nil => results
      case t :: ts if results contains t => loop(ts, results)
      case t :: ts => loop(ts ++ pattern.findAllIn(t, sent, doc, state), t +: results)
    }
    loop(Seq(tok), Nil)
  }
}
