package edu.arizona.sista.matcher

import scala.util.parsing.combinator._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.struct.DirectedGraph

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

trait Values {
  def values(strings: Option[Array[String]], msg: String): Seq[String] =
    strings match {
      case None => sys.error(msg)
      case Some(strings) => strings
    }

  def word(tok: Int, sent: Int, doc: Document): String =
    doc.sentences(sent).words(tok)

  def words(tokens: Seq[Int], sent: Int, doc: Document): Seq[(Int, String)] = {
    val strings = doc.sentences(sent).words
    tokens map (i => (i, strings(i)))
  }

  def lemma(tok: Int, sent: Int, doc: Document): String = {
    val lemmas = values(doc.sentences(sent).lemmas, "sentence has no lemmas")
    lemmas(tok)
  }

  def lemmas(tokens: Seq[Int], sent: Int, doc: Document): Seq[(Int, String)] = {
    val strings = values(doc.sentences(sent).lemmas, "sentence has no lemmas")
    tokens map (i => (i, strings(i)))
  }

  def tag(tok: Int, sent: Int, doc: Document): String = {
    val tags = values(doc.sentences(sent).tags, "sentence has no tags")
    tags(tok)
  }

  def tags(tokens: Seq[Int], sent: Int, doc: Document): Seq[(Int, String)] = {
    val strings = values(doc.sentences(sent).tags, "sentence has no tags")
    tokens map (i => (i, strings(i)))
  }

  def entity(tok: Int, sent: Int, doc: Document): String = {
    val entities = values(doc.sentences(sent).entities, "sentence has no entities")
    entities(tok)
  }

  def entities(tokens: Seq[Int], sent: Int, doc: Document): Seq[(Int, String)] = {
    val strings = values(doc.sentences(sent).entities, "sentence has no entities")
    tokens map (i => (i, strings(i)))
  }

  def chunk(tok: Int, sent: Int, doc: Document): String = {
    val chunks = values(doc.sentences(sent).chunks, "sentence has no chunks")
    chunks(tok)
  }

  def chunks(tokens: Seq[Int], sent: Int, doc: Document): Seq[(Int, String)] = {
    val strings = values(doc.sentences(sent).chunks, "sentence has no chunks")
    tokens map (i => (i, strings(i)))
  }
}

trait Dependencies {
  def dependencies(sent: Int, doc: Document): DirectedGraph[String] =
    doc.sentences(sent).dependencies match {
      case None => sys.error("sentence has no dependencies")
      case Some(deps) => deps
    }

  def incomingEdges(sent: Int, doc: Document): Array[Array[(Int, String)]] =
    dependencies(sent, doc).incomingEdges

  def outgoingEdges(sent: Int, doc: Document): Array[Array[(Int, String)]] =
    dependencies(sent, doc).outgoingEdges

  def incoming(tok: Int, sent: Int, doc: Document): Seq[String] =
    incomingEdges(sent, doc)(tok) map (_._2)

  def outgoing(tok: Int, sent: Int, doc: Document): Seq[String] =
    outgoingEdges(sent, doc)(tok) map (_._2)
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
