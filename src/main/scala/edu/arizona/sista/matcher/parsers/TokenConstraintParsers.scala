package edu.arizona.sista.matcher

import scala.util.parsing.combinator._
import edu.arizona.sista.processors.Document

trait TokenConstraintParsers extends LiteralParsers {
  def tokenConstraint: Parser[TokenConstraint] = "[" ~> disjunctiveConstraint <~ "]"

  def wordConstraint: Parser[TokenConstraint] = stringMatcher ^^ {
    case matcher => new WordConstraint(matcher)
  }

  def disjunctiveConstraint: Parser[TokenConstraint] =
    conjunctiveConstraint ~ rep("|" ~> conjunctiveConstraint) ^^ {
      case first ~ rest => (first /: rest) {
        case (lhs, rhs) => new DisjunctiveConstraint(lhs, rhs)
      }
    }

  def conjunctiveConstraint: Parser[TokenConstraint] =
    negatedConstraint ~ rep("&" ~> negatedConstraint) ^^ {
      case first ~ rest => (first /: rest) {
        case (lhs, rhs) => new ConjunctiveConstraint(lhs, rhs)
      }
    }

  def negatedConstraint: Parser[TokenConstraint] = opt("!") ~ atomicConstraint ^^ {
    case None ~ constraint => constraint
    case Some("!") ~ constraint => new NegatedConstraint(constraint)
  }

  def atomicConstraint: Parser[TokenConstraint] = fieldConstraint | "(" ~> disjunctiveConstraint <~ ")"

  def fieldConstraint: Parser[TokenConstraint] = ident ~ "=" ~ stringMatcher ^^ {
    case "word" ~ _ ~ matcher => new WordConstraint(matcher)
    case "lemma" ~ _ ~ matcher => new LemmaConstraint(matcher)
    case "tag" ~ _ ~ matcher => new TagConstraint(matcher)
    case "entity" ~ _ ~ matcher => new EntityConstraint(matcher)
    case "chunk" ~ _ ~ matcher => new ChunkConstraint(matcher)
    case "incoming" ~ _ ~ matcher => new IncomingConstraint(matcher)
    case "outgoing" ~ _ ~ matcher => new OutgoingConstraint(matcher)
    case _ => sys.error("unrecognized token field")
  }
}

sealed trait TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document): Boolean
  def filter(tokens: Seq[Int], sent: Int, doc: Document): Seq[Int]
}

class WordConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    matcher matches word(tok, sent, doc)

  def filter(tokens: Seq[Int], sent: Int, doc: Document): Seq[Int] =
    matcher filter words(tokens, sent, doc)
}

class LemmaConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    matcher matches lemma(tok, sent, doc)

  def filter(tokens: Seq[Int], sent: Int, doc: Document): Seq[Int] =
    matcher filter lemmas(tokens, sent, doc)
}

class TagConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    matcher matches tag(tok, sent, doc)

  def filter(tokens: Seq[Int], sent: Int, doc: Document): Seq[Int] =
    matcher filter tags(tokens, sent, doc)
}

class EntityConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    matcher matches entity(tok, sent, doc)

  def filter(tokens: Seq[Int], sent: Int, doc: Document): Seq[Int] =
    matcher filter entities(tokens, sent, doc)
}

class ChunkConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    matcher matches chunk(tok, sent, doc)

  def filter(tokens: Seq[Int], sent: Int, doc: Document): Seq[Int] =
    matcher filter chunks(tokens, sent, doc)
}

class IncomingConstraint(matcher: StringMatcher) extends TokenConstraint with Dependencies {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    incoming(tok, sent, doc) exists matcher.matches

  def filter(tokens: Seq[Int], sent: Int, doc: Document): Seq[Int] = {
    val edges = incomingEdges(sent, doc)
    tokens filter (tok => (edges.isDefinedAt(tok) && matcher.filter(edges(tok)).nonEmpty))
  }
}

class OutgoingConstraint(matcher: StringMatcher) extends TokenConstraint with Dependencies {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    outgoing(tok, sent, doc) exists matcher.matches

  def filter(tokens: Seq[Int], sent: Int, doc: Document): Seq[Int] = {
    val edges = outgoingEdges(sent, doc)
    tokens filter (tok => (edges.isDefinedAt(tok) && matcher.filter(edges(tok)).nonEmpty))
  }
}

class NegatedConstraint(constraint: TokenConstraint) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    !constraint.matches(tok, sent, doc)

  def filter(tokens: Seq[Int], sent: Int, doc: Document): Seq[Int] =
    tokens diff constraint.filter(tokens, sent, doc)
}

class ConjunctiveConstraint(lhs: TokenConstraint, rhs: TokenConstraint) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    lhs.matches(tok, sent, doc) && rhs.matches(tok, sent, doc)

  def filter(tokens: Seq[Int], sent: Int, doc: Document): Seq[Int] =
    lhs.filter(tokens, sent, doc) intersect rhs.filter(tokens, sent, doc)
}

class DisjunctiveConstraint(lhs: TokenConstraint, rhs: TokenConstraint) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document): Boolean =
    lhs.matches(tok, sent, doc) || rhs.matches(tok, sent, doc)

  def filter(tokens: Seq[Int], sent: Int, doc: Document): Seq[Int] =
    (lhs.filter(tokens, sent, doc) union rhs.filter(tokens, sent, doc)).distinct
}
