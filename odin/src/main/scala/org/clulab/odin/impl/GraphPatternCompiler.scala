package org.clulab.odin.impl

import org.clulab.processors.Document
import org.clulab.odin._


class GraphPatternCompiler(unit: String, config: OdinConfig) extends TokenPatternParsers(unit, config) {

  def compileGraphPattern(input: String): GraphPattern =
    parseAll(dependencyPattern, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => sys.error(failure.msg)
    }

  def dependencyPattern: Parser[GraphPattern] =
    triggerPatternGraphPattern | triggerMentionGraphPattern

  def triggerPatternGraphPattern: Parser[GraphPattern] =
    "(?i)trigger".r ~> "=" ~> tokenPattern ~ rep1(argPattern) ^^ {
      case trigger ~ arguments => new TriggerPatternGraphPattern(trigger, arguments, config)
    }

  def triggerMentionGraphPattern: Parser[GraphPattern] =
    identifier ~ ":" ~ identifier ~ rep1(argPattern) ^^ {
      case anchorName ~ ":" ~ anchorLabel ~ arguments if anchorName equalsIgnoreCase "trigger" =>
        new TriggerMentionGraphPattern(anchorLabel, arguments, config)
      case anchorName ~ ":" ~ anchorLabel ~ arguments =>
        // if anchorName is not "trigger" then return a RelationMention
        new RelationGraphPattern(anchorName, anchorLabel, arguments, config)
    }

  def argPattern: Parser[ArgumentPattern] =
    identifier ~ ":" ~ identifier ~ opt("?" | "*" | "+" | "{" ~> int <~ "}") ~ "=" ~ disjunctiveGraphPattern ^^ {
      case name ~ _ ~ _ ~ _ ~ _ ~ _ if name equalsIgnoreCase "trigger" =>
        sys.error(s"'$name' is not a valid argument name")
      case name ~ ":" ~ label ~ None ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, size = Some(1), config)
      case name ~ ":" ~ label ~ Some("?") ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = false, size = Some(1), config)
      case name ~ ":" ~ label ~ Some("*") ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = false, size = None, config)
      case name ~ ":" ~ label ~ Some("+") ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, size = None, config)
      case name ~ ":" ~ label ~ Some(n:Int) ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, size = Some(n), config)
    }

  def disjunctiveGraphPattern: Parser[GraphPatternNode] =
    rep1sep(concatGraphPattern, "|") ^^ { chunks =>
      (chunks.head /: chunks.tail) {
        case (lhs, rhs) => new DisjunctiveGraphPattern(lhs, rhs)
      }
    }

  def concatGraphPattern: Parser[GraphPatternNode] =
    rep1(stepGraphPattern) ^^ { chunks =>
      (chunks.head /: chunks.tail) {
        case (lhs, rhs) => new ConcatGraphPattern(lhs, rhs)
      }
    }

  def stepGraphPattern: Parser[GraphPatternNode] =
    filterGraphPattern | traversalGraphPattern

  /** token constraint */
  def filterGraphPattern: Parser[GraphPatternNode] =
    tokenConstraint ^^ { new TokenConstraintGraphPattern(_) }

  /** any pattern that represents graph traversal */
  def traversalGraphPattern: Parser[GraphPatternNode] =
    atomicGraphPattern ||| repeatGraphPattern ||| rangeGraphPattern ||| quantifiedGraphPattern

  def quantifiedGraphPattern: Parser[GraphPatternNode] =
    atomicGraphPattern ~ ("?" | "*" | "+") ^^ {
      case pat ~ "?" => new OptionalGraphPattern(pat)
      case pat ~ "*" => new KleeneGraphPattern(pat)
      case pat ~ "+" => new ConcatGraphPattern(pat, new KleeneGraphPattern(pat))
    }

  // helper function that repeats a pattern N times
  private def repeatPattern(pattern: GraphPatternNode, n: Int): GraphPatternNode = {
    require(n > 0, "'n' must be greater than zero")
    (pattern /: Seq.fill(n - 1)(pattern)) {
      case (lhs, rhs) => new ConcatGraphPattern(lhs, rhs)
    }
  }

  def repeatGraphPattern: Parser[GraphPatternNode] =
    atomicGraphPattern ~ ("{" ~> int <~ "}") ^^ {
      case pat ~ n => repeatPattern(pat, n)
    }

  def rangeGraphPattern: Parser[GraphPatternNode] =
    atomicGraphPattern ~ "{" ~ opt(int) ~ "," ~ opt(int) ~ "}" ^^ {
      case pat ~ "{" ~ from ~ "," ~ to ~ "}" => (from, to) match {
        case (None, None) =>
          sys.error("invalid range")
        case (None, Some(n)) =>
          repeatPattern(new OptionalGraphPattern(pat), n)
        case (Some(m), None) =>
          val req = repeatPattern(pat, m)
          val kleene = new KleeneGraphPattern(pat)
          new ConcatGraphPattern(req, kleene)
        case (Some(m), Some(n)) =>
          require(n > m, "'to' must be greater than 'from'")
          val req = repeatPattern(pat, m)
          val opt = repeatPattern(new OptionalGraphPattern(pat), n - m)
          new ConcatGraphPattern(req, opt)
      }
    }

  def lookaroundGraphPattern: Parser[GraphPatternNode] =
    ("(?=" | "(?!") ~ disjunctiveGraphPattern <~ ")" ^^ {
      case op ~ pat => new LookaroundGraphPattern(pat, op.endsWith("!"))
    }

  def atomicGraphPattern: Parser[GraphPatternNode] =
    outgoingPattern | incomingPattern | lookaroundGraphPattern |
    "(" ~> disjunctiveGraphPattern <~ ")"

  def outgoingPattern: Parser[GraphPatternNode] =
    outgoingMatcher | outgoingWildcard

  def incomingPattern: Parser[GraphPatternNode] =
    incomingMatcher | incomingWildcard

  // there is ambiguity between an outgoingMatcher with an implicit '>'
  // and the name of the next argument, we solve this by ensuring that
  // the outgoingMatcher is not followed by ':'
  def outgoingMatcher: Parser[GraphPatternNode] =
    opt(">") ~> stringMatcher <~ not(":") ^^ { new OutgoingGraphPattern(_) }

  def incomingMatcher: Parser[GraphPatternNode] =
    "<" ~> stringMatcher ^^ { new IncomingGraphPattern(_) }

  def outgoingWildcard: Parser[GraphPatternNode] =
    ">>" ^^^ OutgoingWildcard

  def incomingWildcard: Parser[GraphPatternNode] =
    "<<" ^^^ IncomingWildcard

}

class ArgumentPattern(
    val name: String,
    val label: String,
    val pattern: GraphPatternNode,
    val required: Boolean,
    val size: Option[Int],
    val config: OdinConfig
) {
  // extracts mentions and groups them according to `size`
  def extract(tok: Int, sent: Int, doc: Document, state: State): Seq[Seq[(Mention, SynPath)]] = {
    val matches = for {
      (tok, path) <- pattern.findAllIn(tok, sent, doc, state, config)
      m <- state.mentionsFor(sent, tok, label)
    } yield (m, path.reverse) // paths were collected in reverse
    if (matches.isEmpty) Nil
    else if (size.isDefined) matches.combinations(size.get).toList
    else Seq(matches)
  }
}

sealed trait GraphPatternNode {

  def findAllIn(tok: Int, sent: Int, doc: Document, state: State, config: OdinConfig): Seq[(Int, SynPath)] = {
    findAllIn(tok, sent, doc, state, Nil, config)
  }

  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath,
      config: OdinConfig
  ): Seq[(Int, SynPath)]

  // return distinct results considering Int only and ignoring SynPath
  def distinct(results: Seq[(Int, SynPath)]): Seq[(Int, SynPath)] = {
    var seen: Set[Int] = Set.empty
    for ((tok, path) <- results if !seen.contains(tok)) yield {
      seen += tok // remember tok for next time
      (tok, path)
    }
  }

}

object OutgoingWildcard extends GraphPatternNode with Graph {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath,
      config: OdinConfig
  ): Seq[(Int, SynPath)] = {
    val edges = outgoingEdges(sent, doc, config.graph)
    if (edges isDefinedAt tok) {
      for {
        (nextTok, label) <- edges(tok)
        newPath = (tok, nextTok, label) +: path // path is collected in reverse
      } yield (nextTok, newPath)
    } else Nil
  }
}

object IncomingWildcard extends GraphPatternNode with Graph {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath,
      config: OdinConfig
  ): Seq[(Int, SynPath)] = {
    val edges = incomingEdges(sent, doc, config.graph)
    if (edges isDefinedAt tok) {
      for {
        (nextTok, label) <- edges(tok)
        newPath = (nextTok, tok, label) +: path // path is collected in reverse
      } yield (nextTok, newPath)
    } else Nil
  }
}

class OutgoingGraphPattern(matcher: StringMatcher)
extends GraphPatternNode with Graph {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath,
      config: OdinConfig
  ): Seq[(Int, SynPath)] = {
    val edges = outgoingEdges(sent, doc, config.graph)
    if (edges isDefinedAt tok) {
      for {
        (nextTok, label) <- edges(tok)
        if matcher.matches(label)
        newPath = (tok, nextTok, label) +: path // path is collected in reverse
      } yield (nextTok, newPath)
    } else Nil
  }
}

class IncomingGraphPattern(matcher: StringMatcher)
extends GraphPatternNode with Graph {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath,
      config: OdinConfig
  ): Seq[(Int, SynPath)] = {
    val edges = incomingEdges(sent, doc, config.graph)
    if (edges isDefinedAt tok) {
      for {
        (nextTok, label) <- edges(tok)
        if matcher.matches(label)
        newPath = (nextTok, tok, label) +: path // path is collected in reverse
      } yield (nextTok, newPath)
    } else Nil
  }
}

class ConcatGraphPattern(lhs: GraphPatternNode, rhs: GraphPatternNode)
extends GraphPatternNode {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath,
      config: OdinConfig
  ): Seq[(Int, SynPath)] = {
    val results = for {
      (i, p) <- lhs.findAllIn(tok, sent, doc, state, path, config)
      (j, q) <- rhs.findAllIn(i, sent, doc, state, p, config)
    } yield (j, q)
    distinct(results)
  }
}

class DisjunctiveGraphPattern(lhs: GraphPatternNode, rhs: GraphPatternNode)
extends GraphPatternNode {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath,
      config: OdinConfig
  ): Seq[(Int, SynPath)] = {
    val leftResults = lhs.findAllIn(tok, sent, doc, state, path, config)
    val rightResults = rhs.findAllIn(tok, sent, doc, state, path, config)
    distinct(leftResults ++ rightResults)
  }
}

class TokenConstraintGraphPattern(constraint: TokenConstraint)
extends GraphPatternNode {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath,
      config: OdinConfig
  ): Seq[(Int, SynPath)] = {
    if (constraint.matches(tok, sent, doc, state)) Seq((tok, path)) else Nil
  }
}

class LookaroundGraphPattern(lookaround: GraphPatternNode, negative: Boolean)
extends GraphPatternNode {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath,
      config: OdinConfig
  ): Seq[(Int, SynPath)] = {
    val results = lookaround.findAllIn(tok, sent, doc, state, config)
    if (results.isEmpty == negative) Seq((tok, path)) else Nil
  }
}

class OptionalGraphPattern(pattern: GraphPatternNode)
extends GraphPatternNode {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath,
      config: OdinConfig
  ): Seq[(Int, SynPath)] = {
    distinct((tok, path) +: pattern.findAllIn(tok, sent, doc, state, path, config))
  }
}

class KleeneGraphPattern(pattern: GraphPatternNode)
extends GraphPatternNode {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath,
      config: OdinConfig
  ): Seq[(Int, SynPath)] = {
    @annotation.tailrec
    def collect(
        remaining: Seq[(Int, SynPath)],
        seen: Set[Int],
        results: Seq[(Int, SynPath)]
    ): Seq[(Int, SynPath)] = remaining match {
      case Seq() => results
      case (t, p) +: rest if seen contains t => collect(rest, seen, results)
      case (t, p) +: rest =>
        collect(rest ++ pattern.findAllIn(t, sent, doc, state, p, config), seen + t, (t, p) +: results)
    }
    collect(Seq((tok, path)), Set.empty, Nil)
  }
}
