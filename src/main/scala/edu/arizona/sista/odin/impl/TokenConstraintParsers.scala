package edu.arizona.sista.odin.impl

import scala.util.parsing.combinator._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin._

trait TokenConstraintParsers extends StringMatcherParsers {

  def tokenConstraint: Parser[TokenConstraint] =
    "[" ~> opt(disjunctiveConstraint) <~ "]" ^^ {
      case Some(constraint) => constraint
      case None => TokenWildcard
    }

  /** the field that should be used by unitConstraint */
  def unit: String

  /** makes a token constraint from a single string matcher */
  def unitConstraint: Parser[TokenConstraint] = stringMatcher ^^ {
    case matcher => unit match {
      case "word" => new WordConstraint(matcher)
      case "tag" => new TagConstraint(matcher)
      case _ => sys.error("unrecognized token field")
    }
  }

  def disjunctiveConstraint: Parser[TokenConstraint] =
    rep1sep(conjunctiveConstraint, "|") ^^ { chunks =>
      (chunks.head /: chunks.tail) {
        case (lhs, rhs) => new DisjunctiveConstraint(lhs, rhs)
      }
    }

  def conjunctiveConstraint: Parser[TokenConstraint] =
    rep1sep(negatedConstraint, "&") ^^ { chunks =>
      (chunks.head /: chunks.tail) {
        case (lhs, rhs) => new ConjunctiveConstraint(lhs, rhs)
      }
    }

  def negatedConstraint: Parser[TokenConstraint] = opt("!") ~ atomicConstraint ^^ {
    case None ~ constraint => constraint
    case Some("!") ~ constraint => new NegatedConstraint(constraint)
  }

  def atomicConstraint: Parser[TokenConstraint] =
    fieldConstraint | "(" ~> disjunctiveConstraint <~ ")"

  def fieldConstraint: Parser[TokenConstraint] = identifier ~ "=" ~ stringMatcher ^^ {
    case "word" ~ _ ~ matcher => new WordConstraint(matcher)
    case "lemma" ~ _ ~ matcher => new LemmaConstraint(matcher)
    case "tag" ~ _ ~ matcher => new TagConstraint(matcher)
    case "entity" ~ _ ~ matcher => new EntityConstraint(matcher)
    case "chunk" ~ _ ~ matcher => new ChunkConstraint(matcher)
    case "incoming" ~ _ ~ matcher => new IncomingConstraint(matcher)
    case "outgoing" ~ _ ~ matcher => new OutgoingConstraint(matcher)
    case "mention" ~ _ ~ matcher => new MentionConstraint(matcher)
    case _ => sys.error("unrecognized token field")
  }

}

sealed trait TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean
}

/** matches any token, provided that it exists */
object TokenWildcard extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    doc.sentences(sent).words.isDefinedAt(tok)
}

class WordConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    matcher matches word(tok, sent, doc)
}

class LemmaConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    matcher matches lemma(tok, sent, doc)
}

class TagConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    matcher matches tag(tok, sent, doc)
}

class EntityConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    matcher matches entity(tok, sent, doc)
}

class ChunkConstraint(matcher: StringMatcher) extends TokenConstraint with Values {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    matcher matches chunk(tok, sent, doc)
}

class IncomingConstraint(matcher: StringMatcher) extends TokenConstraint with Dependencies {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    incoming(tok, sent, doc) exists matcher.matches
}

class OutgoingConstraint(matcher: StringMatcher) extends TokenConstraint with Dependencies {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    outgoing(tok, sent, doc) exists matcher.matches
}

// checks that a token is inside a mention
class MentionConstraint(matcher: StringMatcher) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    state.mentionsFor(sent, tok) exists (_ matches matcher)
}

class NegatedConstraint(constraint: TokenConstraint) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    !constraint.matches(tok, sent, doc, state)
}

class ConjunctiveConstraint(lhs: TokenConstraint, rhs: TokenConstraint) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.matches(tok, sent, doc, state) && rhs.matches(tok, sent, doc, state)
}

class DisjunctiveConstraint(lhs: TokenConstraint, rhs: TokenConstraint) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.matches(tok, sent, doc, state) || rhs.matches(tok, sent, doc, state)
}
