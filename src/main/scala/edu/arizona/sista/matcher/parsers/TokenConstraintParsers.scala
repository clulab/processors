package edu.arizona.sista.matcher

import scala.util.parsing.combinator._
import edu.arizona.sista.processors.Document

trait TokenConstraintParsers extends LiteralParsers {
  def tokenConstraint: Parser[TokenConstraint] = "[" ~> disjunctiveConstraint <~ "]"

  def wordConstraint: Parser[TokenConstraint] = stringMatcher ^^ {
    case matcher => new WordConstraint(matcher)
  }

  def disjunctiveConstraint: Parser[TokenConstraint] = conjunctiveConstraint ~ rep("|" ~> conjunctiveConstraint) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new DisjunctiveConstraint(lhs, rhs)
    }
  }

  def conjunctiveConstraint: Parser[TokenConstraint] = negatedConstraint ~ rep("&" ~> negatedConstraint) ^^ {
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
    case _ => sys.error("unrecognized token field")
  }
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
