package edu.arizona.sista.odin.impl

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
    numericConstraint | fieldConstraint | "(" ~> disjunctiveConstraint <~ ")"

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

  /** for numerical comparisons */
  def numberExpression: Parser[NumericExpression] = addSubtractExpression

  def addSubtractExpression: Parser[NumericExpression] = additionExpression | subtractionExpression

  def additionExpression: Parser[NumericExpression] = rep1sep(productExpression, "+") ^^ { prods =>
    (prods.head /: prods.tail) {
      case (lhs, rhs) => new Addition(lhs, rhs)
    }
  }

  def subtractionExpression: Parser[NumericExpression] = rep1sep(productExpression, "+") ^^ { prods =>
    (prods.head /: prods.tail) {
      case (lhs, rhs) => new Addition(lhs, rhs)
    }
  }

  def productExpression: Parser[NumericExpression] = multiplicationExpression | divisionExpression | truncatedDivisionExpression | moduloExpression

  def multiplicationExpression: Parser[NumericExpression] = rep1sep(termExpression, "*") ^^ { prods =>
    (prods.head /: prods.tail) {
      case (lhs, rhs) => new Multiplication(lhs, rhs)
    }
  }

  def divisionExpression: Parser[NumericExpression] = rep1sep(termExpression, "/") ^^ { prods =>
    (prods.head /: prods.tail) {
      case (lhs, rhs) => new Division(lhs, rhs)
    }
  }

  def truncatedDivisionExpression: Parser[NumericExpression] = rep1sep(termExpression, "//") ^^ { prods =>
    (prods.head /: prods.tail) {
      case (lhs, rhs) => new TruncatedDivision(lhs, rhs)
    }
  }

  def moduloExpression: Parser[NumericExpression] = rep1sep(termExpression, "%") ^^ { prods =>
    (prods.head /: prods.tail) {
      case (lhs, rhs) => new Modulo(lhs, rhs)
    }
  }

  def termExpression: Parser[NumericExpression] = numberLiteral | numericFunction | "(" ~> numberExpression <~ ")"

  // an equality symbol
  // the longest strings must come first
  def compareOps: Parser[String] = ">=" | "<=" | "==" | "!="| ">" | "<"

  def numberLiteral: Parser[NumericExpression] =
    """[-+]?(?:\d*\.?\d+|\d+\.?\d*)(?:[eE][-+]?\d+)?""".r ^^ { num =>
      new Literal(num.toDouble)
    }

  /** update as needed.  Currently only a distributional similarity comparison */
  def numericFunction: Parser[NumericExpression] = similarTo

  def numericConstraint: Parser[TokenConstraint] = numberExpression ~ compareOps ~ numberExpression ^^ {
    case lhs ~ ">" ~ rhs => new GreaterThan(lhs, rhs)
    case lhs ~ ">=" ~ rhs => new GreaterThanOrEqual(lhs, rhs)
    case lhs ~ "<" ~ rhs => new LessThan(lhs, rhs)
    case lhs ~ "<=" ~ rhs => new LessThanOrEqual(lhs, rhs)
    case lhs ~ "==" ~ rhs => new Equal(lhs, rhs)
    case lhs ~ "!=" ~ rhs => new NotEqual(lhs, rhs)
  }

  /**
   * ex. get the distributional similarity score (dot product of L2 normed vectors)
   * for a specified word and the current token
   * */
  def similarTo: Parser[NumericExpression] = "simTo" ~> "(" ~> stringLiteral <~ ")" ^^ {
    case w2 if resources.embeddings.nonEmpty => new SimilarityConstraint(w2, resources.embeddings.get)
    case missing if resources.embeddings.isEmpty =>
      throw new OdinCompileException("Error compiling pattern using 'simTo'. No embeddings specified in 'resources'")
  }

}


sealed trait TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean
}

/** for numerical comparisons */
sealed trait NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double
}

class Literal(val num: Double) extends NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double = num
}

class Addition(lhs: NumericExpression, rhs: NumericExpression) extends NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double =
    lhs.number(tok, sent, doc, state) + rhs.number(tok, sent, doc, state)
}

class Multiplication(lhs: NumericExpression, rhs: NumericExpression) extends NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double =
    lhs.number(tok, sent, doc, state) * rhs.number(tok, sent, doc, state)
}

class Division(lhs: NumericExpression, rhs: NumericExpression) extends NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double =
    lhs.number(tok, sent, doc, state) / rhs.number(tok, sent, doc, state)
}

class TruncatedDivision(lhs: NumericExpression, rhs: NumericExpression) extends NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double =
    (lhs.number(tok, sent, doc, state) / rhs.number(tok, sent, doc, state)).floor
}

class Modulo(lhs: NumericExpression, rhs: NumericExpression) extends NumericExpression {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double =
    lhs.number(tok, sent, doc, state) % rhs.number(tok, sent, doc, state)
}

/** matcher must be an exact string matcher, so that a particular word vector can be retrieved */
class SimilarityConstraint(w1: String, embeddings: EmbeddingsResource) extends NumericExpression with Values {
  def number(tok: Int, sent: Int, doc: Document, state: State): Double = {
    // current word
    val w2 = word(tok, sent, doc)

    // no need to check to see if w1 & w2 in embeddings
    // this is handled by the EmbeddingsResource
    embeddings.similarity(w1, w2)
  }
}

class GreaterThan(lhs: NumericExpression, rhs: NumericExpression) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.number(tok, sent, doc, state) > rhs.number(tok, sent, doc, state)
}

class LessThan(lhs: NumericExpression, rhs: NumericExpression) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.number(tok, sent, doc, state) < rhs.number(tok, sent, doc, state)
}

class GreaterThanOrEqual(lhs: NumericExpression, rhs: NumericExpression) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.number(tok, sent, doc, state) >= rhs.number(tok, sent, doc, state)
}

class LessThanOrEqual(lhs: NumericExpression, rhs: NumericExpression) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.number(tok, sent, doc, state) <= rhs.number(tok, sent, doc, state)
}

class Equal(lhs: NumericExpression, rhs: NumericExpression) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.number(tok, sent, doc, state) == rhs.number(tok, sent, doc, state)
}

class NotEqual(lhs: NumericExpression, rhs: NumericExpression) extends TokenConstraint {
  def matches(tok: Int, sent: Int, doc: Document, state: State): Boolean =
    lhs.number(tok, sent, doc, state) != rhs.number(tok, sent, doc, state)
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
