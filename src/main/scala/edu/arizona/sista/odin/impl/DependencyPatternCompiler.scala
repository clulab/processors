package edu.arizona.sista.odin.impl

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin._

class DependencyPatternCompiler(unit: String) extends TokenPatternParsers(unit) {

  def compileDependencyPattern(input: String): DependencyPattern =
    parseAll(dependencyPattern, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => sys.error(failure.msg)
    }

  def dependencyPattern: Parser[DependencyPattern] =
    eventDependencyPattern | relationDependencyPattern

  def eventDependencyPattern: Parser[DependencyPattern] =
    "(?i)trigger".r ~> "=" ~> tokenPattern ~ rep1(argPattern) ^^ {
      case trigger ~ arguments => new EventDependencyPattern(trigger, arguments)
    }

  def relationDependencyPattern: Parser[DependencyPattern] =
    identifier ~ ":" ~ identifier ~ rep1(argPattern) ^^ {
      case name ~ _ ~ _ ~ _ if name.equalsIgnoreCase("trigger") =>
        sys.error("'trigger' is not a valid argument name")
      case anchorName ~ ":" ~ anchorLabel ~ arguments =>
        new RelationDependencyPattern(anchorName, anchorLabel, arguments)
    }

  def argPattern: Parser[ArgumentPattern] =
    singleArgPattern ||| quantifiedArgPattern ||| exactSizeArgPattern

  def singleArgPattern: Parser[ArgumentPattern] =
    identifier ~ ":" ~ identifier ~ "=" ~ disjunctiveDepPattern ^^ {
      case name ~ _ ~ _ ~ _ ~ _ if name.equalsIgnoreCase("trigger") =>
        sys.error("'trigger' is not a valid argument name")
      case name ~ ":" ~ label ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, size = Some(1))
    }

  def quantifiedArgPattern: Parser[ArgumentPattern] =
    identifier ~ ":" ~ identifier ~ ("?" | "*" | "+") ~ "=" ~ disjunctiveDepPattern ^^ {
      case name ~ _ ~ _ ~ _ ~ _ ~ _ if name.equalsIgnoreCase("trigger") =>
        sys.error("'trigger' is not a valid argument name")
      case name ~ ":" ~ label ~ "?" ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = false, size = Some(1))
      case name ~ ":" ~ label ~ "*" ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = false, size = None)
      case name ~ ":" ~ label ~ "+" ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, size = None)
    }

  def exactSizeArgPattern: Parser[ArgumentPattern] =
    identifier ~ ":" ~ identifier ~ ("{" ~> int <~ "}") ~ "=" ~ disjunctiveDepPattern ^^ {
      case name ~ _ ~ _ ~ _ ~ _ ~ _ if name.equalsIgnoreCase("trigger") =>
        sys.error("'trigger' is not a valid argument name")
      case name ~ ":" ~ label ~ n ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, size = Some(n))
    }

  def disjunctiveDepPattern: Parser[DependencyPatternNode] =
    rep1sep(concatDepPattern, "|") ^^ { chunks =>
      (chunks.head /: chunks.tail) {
        case (lhs, rhs) => new DisjunctiveDependencyPattern(lhs, rhs)
      }
    }

  def concatDepPattern: Parser[DependencyPatternNode] =
    rep1(stepDepPattern) ^^ { chunks =>
      (chunks.head /: chunks.tail) {
        case (lhs, rhs) => new ConcatDependencyPattern(lhs, rhs)
      }
    }

  def stepDepPattern: Parser[DependencyPatternNode] =
    filterDepPattern | traversalDepPattern

  /** token constraint */
  def filterDepPattern: Parser[DependencyPatternNode] =
    tokenConstraint ^^ { new TokenConstraintDependencyPattern(_) }

  /** any pattern that represents graph traversal */
  def traversalDepPattern: Parser[DependencyPatternNode] =
    atomicDepPattern ||| repeatDepPattern ||| rangeDepPattern ||| quantifiedDepPattern

  def quantifiedDepPattern: Parser[DependencyPatternNode] =
    atomicDepPattern ~ ("?" | "*" | "+") ^^ {
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
    atomicDepPattern ~ ("{" ~> int <~ "}") ^^ {
      case pat ~ n => repeatPattern(pat, n)
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

  def lookaroundDepPattern: Parser[DependencyPatternNode] =
    ("(?=" | "(?!") ~ disjunctiveDepPattern <~ ")" ^^ {
      case op ~ pat => new LookaroundDependencyPattern(pat, op.endsWith("!"))
    }

  def atomicDepPattern: Parser[DependencyPatternNode] =
    outgoingPattern | incomingPattern | lookaroundDepPattern |
    "(" ~> disjunctiveDepPattern <~ ")"

  def outgoingPattern: Parser[DependencyPatternNode] =
    outgoingMatcher | outgoingWildcard

  def incomingPattern: Parser[DependencyPatternNode] =
    incomingMatcher | incomingWildcard

  // there is ambiguity between an outgoingMatcher with an implicit '>'
  // and the name of the next argument, we solve this by ensuring that
  // the outgoingMatcher is not followed by ':'
  def outgoingMatcher: Parser[DependencyPatternNode] =
    opt(">") ~> stringMatcher <~ not(":") ^^ { new OutgoingDependencyPattern(_) }

  def incomingMatcher: Parser[DependencyPatternNode] =
    "<" ~> stringMatcher ^^ { new IncomingDependencyPattern(_) }

  def outgoingWildcard: Parser[DependencyPatternNode] =
    ">>" ^^^ OutgoingWildcard

  def incomingWildcard: Parser[DependencyPatternNode] =
    "<<" ^^^ IncomingWildcard

}

class ArgumentPattern(
    val name: String,
    val label: String,
    val pattern: DependencyPatternNode,
    val required: Boolean,
    val size: Option[Int]
) {
  // extracts mentions and groups them according to `size`
  def extract(tok: Int, sent: Int, doc: Document, state: State): Seq[Seq[Mention]] = {
    val matches = for {
      t <- pattern.findAllIn(tok, sent, doc, state)
      m <- state.mentionsFor(sent, t, label)
    } yield m
    if (matches.isEmpty) Nil
    else if (size.isDefined) matches.combinations(size.get).toList
    else Seq(matches)
  }
}

sealed trait DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Int]
}

object OutgoingWildcard extends DependencyPatternNode with Dependencies {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Int] = {
    val edges = outgoingEdges(sent, doc)
    if (edges isDefinedAt tok) edges(tok).map(_._1)
    else Nil
  }
}

object IncomingWildcard extends DependencyPatternNode with Dependencies {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Int] = {
    val edges = incomingEdges(sent, doc)
    if (edges isDefinedAt tok) edges(tok).map(_._1)
    else Nil
  }
}

class OutgoingDependencyPattern(matcher: StringMatcher)
extends DependencyPatternNode with Dependencies {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Int] = {
    val edges = outgoingEdges(sent, doc)
    if (edges isDefinedAt tok)
      for ((tok2, label) <- edges(tok) if matcher.matches(label)) yield tok2
    else Nil
  }
}

class IncomingDependencyPattern(matcher: StringMatcher)
extends DependencyPatternNode with Dependencies {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Int] = {
    val edges = incomingEdges(sent, doc)
    if (edges isDefinedAt tok)
      for ((tok2, label) <- edges(tok) if matcher.matches(label)) yield tok2
    else Nil
  }
}

class ConcatDependencyPattern(lhs: DependencyPatternNode, rhs: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Int] = {
    val results = for {
      i <- lhs.findAllIn(tok, sent, doc, state)
      j <- rhs.findAllIn(i, sent, doc, state)
    } yield j
    results.distinct
  }
}

class DisjunctiveDependencyPattern(lhs: DependencyPatternNode, rhs: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Int] =
    (lhs.findAllIn(tok, sent, doc, state) ++ rhs.findAllIn(tok, sent, doc, state)).distinct
}

class TokenConstraintDependencyPattern(constraint: TokenConstraint)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Int] =
    if (constraint.matches(tok, sent, doc, state)) Seq(tok) else Nil
}

class LookaroundDependencyPattern(lookaround: DependencyPatternNode, negative: Boolean)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Int] = {
    val results = lookaround.findAllIn(tok, sent, doc, state)
    if (results.isEmpty == negative) Seq(tok) else Nil
  }
}

class OptionalDependencyPattern(pattern: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Int] =
    (tok +: pattern.findAllIn(tok, sent, doc, state)).distinct
}

class KleeneDependencyPattern(pattern: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Int] = {
    @annotation.tailrec
    def loop(remaining: Seq[Int], results: Seq[Int]): Seq[Int] = remaining match {
      case Seq() => results
      case t +: ts if results contains t => loop(ts, results)
      case t +: ts => loop(ts ++ pattern.findAllIn(t, sent, doc, state), t +: results)
    }
    loop(Seq(tok), Nil)
  }
}
