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
    javaIdentifier ~ ":" ~ javaIdentifier ~ rep1(argPattern) ^^ {
      case anchorName ~ ":" ~ anchorLabel ~ arguments if anchorName equalsIgnoreCase "trigger" =>
        new TriggerMentionGraphPattern(anchorLabel, arguments, config)
      case anchorName ~ ":" ~ anchorLabel ~ arguments =>
        // if anchorName is not "trigger" then return a RelationMention
        new RelationGraphPattern(anchorName, anchorLabel, arguments, config)
    }

  def argPattern: Parser[ArgumentPattern] =
    javaIdentifier ~ ":" ~ javaIdentifier ~ opt("?" ||| "*" ||| "*?" ||| "+" ||| "+?" |||
      "{" ~> int <~ "}" |||
      "{" ~ opt(int) ~ "," ~ opt(int) ~ ( "}" ||| "}?" )
    ) ~ "=" ~ disjunctiveGraphPattern ^^ {
      case name ~ _ ~ _ ~ _ ~ _ ~ _ if name equalsIgnoreCase "trigger" =>
        sys.error(s"'$name' is not a valid argument name")
      // no quantifier
      case name ~ ":" ~ label ~ None ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, quantifier = NullQuantifier, config)
      // optional
      case name ~ ":" ~ label ~ Some("?") ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = false, quantifier = RangedQuantifier(None, Some(1)), config)
      // Kleene star
      case name ~ ":" ~ label ~ Some("*") ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = false, quantifier = RangedQuantifier(None, None), config)
      // Don't allow lazy Kleene star for args
      case name ~ ":" ~ label ~ Some("*?") ~ "=" ~ pat =>
        throw OdinCompileException(s"Lazy Kleene star (*?) used for argument '$name'.  Remove argument pattern from rule.")
      // one or more (greedy)
      case name ~ ":" ~ label ~ Some("+") ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, quantifier = RangedQuantifier(Some(1), None), config)
      // one or more (lazy)
      // NOTE: instead of throwing an exception, a warning could be printed and the +? could simply be dropped in compilation
      case name ~ ":" ~ label ~ Some("+?") ~ "=" ~ pat =>
        throw OdinCompileException(s"+? used for argument '$name', but it is superfluous in this context. Remove +? quantifier from argument.")
      // exact count
      case name ~ ":" ~ label ~ Some(exact: Int) ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, quantifier = ExactQuantifier(exact), config)
      // open range quantifier
      // make combinations of the largest value found in the range (i.e., in {2,5} if 5 matches found, create combos of 5)
      case name ~ ":" ~ label ~ Some( "{" ~ Some(minRep: Int) ~ "," ~ None ~ "}" ) ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, quantifier = RangedQuantifier(minRepeat = Some(minRep), maxRepeat = None), config)
      case name ~ ":" ~ label ~ Some( "{" ~ None ~ "," ~ Some(maxRep: Int) ~ "}" ) ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = false, quantifier = RangedQuantifier(minRepeat = None, maxRepeat = Some(maxRep)), config)
      // closed range quantifier
      case name ~ ":" ~ label ~ Some( "{" ~ Some(minRep: Int) ~  "," ~ Some(maxRep: Int) ~ "}" ) ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, quantifier = RangedQuantifier(minRepeat = Some(minRep), maxRepeat = None), config)
      // better errors
      case name ~ ":" ~ label ~ Some( _ ~ "}?" ) ~ "=" ~ pat =>
        throw OdinCompileException("? used to modify a ranged quantifier for a graph pattern argument.  Use an exact value (ex. {3} for 3)")
    }

  def disjunctiveGraphPattern: Parser[GraphPatternNode] =
    rep1sep(concatGraphPattern, "|") ^^ { chunks =>
      chunks.tail.foldLeft(chunks.head) {
        case (lhs, rhs) => new DisjunctiveGraphPattern(lhs, rhs)
      }
    }

  def concatGraphPattern: Parser[GraphPatternNode] =
    rep1(stepGraphPattern) ^^ { chunks =>
      chunks.tail.foldLeft(chunks.head) {
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
      case _ => sys.error("unrecognized quantifiedGraphPattern operator")
    }

  // helper function that repeats a pattern N times
  private def repeatPattern(pattern: GraphPatternNode, n: Int): GraphPatternNode = {
    require(n > 0, "'n' must be greater than zero")
    Seq.fill(n - 1)(pattern).foldLeft(pattern) {
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
        case (None | Some(0), None) =>
          new KleeneGraphPattern(pat)
        case (None | Some(0), Some(n)) =>
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
      case _ => sys.error("unrecognized rangeGraphPattern")
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
  // OR
  // in case the arg was written as name:label (which looks like an odinIdentifier)
  // we need to ensure it is not followed by '=' (with an optional argument quantifier in between)
  def outgoingMatcher: Parser[GraphPatternNode] =
    opt(">") ~> stringMatcher <~ not(":" | opt(argQuantifier) ~ "=") ^^ { new OutgoingGraphPattern(_) }

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
    val quantifier: ArgumentQuantifier,
    val config: OdinConfig
) {
  // extracts mentions and groups them according to `size`
  def extract(tok: Int, sent: Int, doc: Document, state: State): Seq[Seq[(Mention, SynPath)]] = {
    val matches = for {
      (tok, path) <- pattern.findAllIn(tok, sent, doc, state, config)
      m <- state.mentionsFor(sent, tok, label)
    } yield (m, path.reverse) // paths were collected in reverse
    (matches, quantifier) match {
      // no matches
      case (Nil, _) => Nil
      // no quantifier w/ some matches
      case (_, NullQuantifier) => matches.combinations(1).toList
      // Kleene star (greedy)
      case (_, RangedQuantifier(None, None)) => Seq(matches)
      // exact
      case (_, ExactQuantifier(exact)) => matches.combinations(exact).toList
      // ranged quantifier w/ min and max reps
      case (_, RangedQuantifier(Some(minRep), Some(maxRep))) =>
        if (matches.size < minRep) Nil
        else if (matches.size > maxRep) matches.combinations(maxRep).toList
        else Seq(matches)
      // ranged quantifier w/ min reps || One or more
      case (_, RangedQuantifier(Some(minRep), None)) =>
        if (matches.size < minRep) Nil else Seq(matches)
      // ranged quantifier w/ max reps  || optional (?)
      case (_, RangedQuantifier(None, Some(maxRep))) =>
        if (matches.size > maxRep) matches.combinations(maxRep).toList
        else Seq(matches)
      case _ => sys.error("unrecognized argument quantifier")
    }
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
