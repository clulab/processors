package edu.arizona.sista.matcher

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document

trait DependencyPatternParsers extends TokenPatternParsers {
  override val whiteSpace = """[ \t\x0B\f\r]+""".r
  val eol = "\n"

  def dependencyPattern: Parser[DependencyPattern] =
    triggerFinder ~ rep1(eol) ~ repsep(argPattern, rep1(eol)) ^^ {
      case trigger ~ _ ~ arguments => new DependencyPattern(trigger, arguments)
    }

  def triggerFinder: Parser[TokenPattern] = "trigger" ~> ":" ~> tokenPattern

  def argPattern: Parser[ArgumentPattern] =
    ident ~ opt("?") ~ ":" ~ disjunctiveDepPattern ^^ {
      case "trigger" ~ _ ~ _ ~ _ => sys.error("`trigger` is not a valid argument name")
      case name ~ None ~ _ ~ pat => new ArgumentPattern(name, pat, true)
      case name ~ Some("?") ~ _ ~ pat => new ArgumentPattern(name, pat, false)
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

  def filteredDepPattern: Parser[DependencyPatternNode] =
    quantifiedDepPattern ~ opt(tokenConstraint) ^^ {
      case pat ~ None => pat
      case pat ~ Some(constraint) => new FilteredDependencyPattern(pat, constraint)
    }

  def quantifiedDepPattern: Parser[DependencyPatternNode] =
    atomicDepPattern ~ opt("?"|"*"|"+") ^^ {
      case pat ~ None => pat
      case pat ~ Some("?") => new OptionalDependencyPattern(pat)
      case pat ~ Some("*") => new KleeneDependencyPattern(pat)
      case pat ~ Some("+") => new ConcatDependencyPattern(pat, new KleeneDependencyPattern(pat))
    }

  def atomicDepPattern: Parser[DependencyPatternNode] =
    outgoingDepPattern | incomingDepPattern | "(" ~> disjunctiveDepPattern <~ ")"

  def outgoingDepPattern: Parser[DependencyPatternNode] =
    opt(">") ~> stringMatcher ^^ { new OutgoingDependencyPattern(_) }

  def incomingDepPattern: Parser[DependencyPatternNode] =
    "<" ~> stringMatcher ^^ { new IncomingDependencyPattern(_) }
}

class ArgumentPattern(val name: String, pattern: DependencyPatternNode, val required: Boolean) {
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
