package org.clulab.odin.impl

import org.clulab.processors.Document
import org.clulab.odin._


class DependencyPatternCompiler(unit: String, resources: OdinResourceManager) extends TokenPatternParsers(unit, resources) {

  def compileDependencyPattern(input: String): DependencyPattern =
    parseAll(dependencyPattern, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => sys.error(failure.msg)
    }

  def dependencyPattern: Parser[DependencyPattern] =
    triggerPatternDependencyPattern | triggerMentionDependencyPattern

  def triggerPatternDependencyPattern: Parser[DependencyPattern] =
    "(?i)trigger".r ~> "=" ~> tokenPattern ~ rep1(argPattern) ^^ {
      case trigger ~ arguments => new TriggerPatternDependencyPattern(trigger, arguments)
    }

  def triggerMentionDependencyPattern: Parser[DependencyPattern] =
    identifier ~ ":" ~ identifier ~ rep1(argPattern) ^^ {
      case anchorName ~ ":" ~ anchorLabel ~ arguments if anchorName equalsIgnoreCase "trigger" =>
        new TriggerMentionDependencyPattern(anchorLabel, arguments)
      case anchorName ~ ":" ~ anchorLabel ~ arguments =>
        // if anchorName is not "trigger" then return a RelationMention
        new RelationDependencyPattern(anchorName, anchorLabel, arguments)
    }

  def argPattern: Parser[ArgumentPattern] =
    identifier ~ ":" ~ identifier ~ opt("?" ||| "*" ||| "*?" ||| "+" ||| "+?" |||
      "{" ~> int <~ "}" |||
      "{" ~ opt(int) ~ "," ~ opt(int) ~ ( "}" ||| "}?" )
    ) ~ "=" ~ disjunctiveDepPattern ^^ {
      case name ~ _ ~ _ ~ _ ~ _ ~ _ if name equalsIgnoreCase "trigger" =>
        sys.error(s"'$name' is not a valid argument name")
      // no quantifier
      case name ~ ":" ~ label ~ None ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, quantifier = NullQuantifier)
      // optional
      case name ~ ":" ~ label ~ Some("?") ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = false, quantifier = RangedQuantifier(None, Some(1)))
      // Kleene star
      case name ~ ":" ~ label ~ Some("*") ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = false, quantifier = RangedQuantifier(None, None))
      // Don't allow lazy Kleene star for args
      case name ~ ":" ~ label ~ Some("*?") ~ "=" ~ pat =>
        throw OdinCompileException(s"Lazy Kleene star (*?) used for argument '$name'.  Remove argument pattern from rule.")
      // one or more (greedy)
      case name ~ ":" ~ label ~ Some("+") ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, quantifier = RangedQuantifier(Some(1), None))
      // one or more (lazy)
      // NOTE: instead of throwing an exception, a warning could be printed and the +? could simply be dropped in compilation
      case name ~ ":" ~ label ~ Some("+?") ~ "=" ~ pat =>
        throw OdinCompileException(s"+? used for argument '$name', but it is superfluous in this context. Remove +? quantifier from argument.")
      // exact count
      case name ~ ":" ~ label ~ Some(exact: Int) ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, quantifier = ExactQuantifier(exact))
      // open range quantifier
      // make combinations of the largest value found in the range (i.e., in {2,5} if 5 matches found, create combos of 5)
      case name ~ ":" ~ label ~ Some( "{" ~ Some(minRep: Int) ~ "," ~ None ~ "}" ) ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, quantifier = RangedQuantifier(minRepeat = Some(minRep), maxRepeat = None))
      case name ~ ":" ~ label ~ Some( "{" ~ None ~ "," ~ Some(maxRep: Int) ~ "}" ) ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = false, quantifier = RangedQuantifier(minRepeat = None, maxRepeat = Some(maxRep)))
      // closed range quantifier
      case name ~ ":" ~ label ~ Some( "{" ~ Some(minRep: Int) ~  "," ~ Some(maxRep: Int) ~"}" ) ~ "=" ~ pat =>
        new ArgumentPattern(name, label, pat, required = true, quantifier = RangedQuantifier(minRepeat = Some(minRep), maxRepeat = None))
      // better errors
      case name ~ ":" ~ label ~ Some( _ ~ "}?" ) ~ "=" ~ pat =>
        throw OdinCompileException("? used to modify a ranged quantifier for a graph pattern argument.  Use an exact value (ex. {3} for 3)")
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
    val quantifier: ArgumentQuantifier
) {
  // extracts mentions and groups them according to `size`
  def extract(tok: Int, sent: Int, doc: Document, state: State): Seq[Seq[(Mention, SynPath)]] = {
    val matches = for {
      (tok, path) <- pattern.findAllIn(tok, sent, doc, state)
      m <- state.mentionsFor(sent, tok, label)
    } yield (m, path.reverse) // paths were collected in reverse
    (matches, quantifier) match {
      // no matches
      case (Nil, _) => Nil
      // no quantifier w/ some matches
      case (_, NullQuantifier) => matches.combinations(1).toList
      // optional
      case (_, RangedQuantifier(None, Some(1))) => matches.combinations(1).toList // at most one per mention
      // Kleene star (greedy)
      case (_, RangedQuantifier(None, None)) => Seq(matches)
      // One or more
      case (_, RangedQuantifier(Some(1), None)) => Seq(matches)
      // exact
      case (_, ExactQuantifier(exact)) => matches.combinations(exact).toList
      // ranged quantifier w/ min and max reps
      case (_, RangedQuantifier(Some(minRep), Some(maxRep))) =>
        if (matches.size < minRep) Nil
        else if (matches.size > maxRep) matches.combinations(maxRep).toList
        else Seq(matches)
      // ranged quantifier w/ min reps
      case (_, RangedQuantifier(Some(minRep), None)) =>
        if (matches.size < minRep) Nil else Seq(matches)
      // ranged quantifier w/ max reps
      case (_, RangedQuantifier(None, Some(maxRep))) =>
        if (matches.size > maxRep) matches.combinations(maxRep).toList
        else Seq(matches)
    }
  }
}


sealed trait DependencyPatternNode {

  def findAllIn(tok: Int, sent: Int, doc: Document, state: State): Seq[(Int, SynPath)] = {
    findAllIn(tok, sent, doc, state, Nil)
  }

  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath
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

object OutgoingWildcard extends DependencyPatternNode with Dependencies {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath
  ): Seq[(Int, SynPath)] = {
    val edges = outgoingEdges(sent, doc)
    if (edges isDefinedAt tok) {
      for {
        (nextTok, label) <- edges(tok)
        newPath = (tok, nextTok, label) +: path // path is collected in reverse
      } yield (nextTok, newPath)
    } else Nil
  }
}

object IncomingWildcard extends DependencyPatternNode with Dependencies {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath
  ): Seq[(Int, SynPath)] = {
    val edges = incomingEdges(sent, doc)
    if (edges isDefinedAt tok) {
      for {
        (nextTok, label) <- edges(tok)
        newPath = (nextTok, tok, label) +: path // path is collected in reverse
      } yield (nextTok, newPath)
    } else Nil
  }
}

class OutgoingDependencyPattern(matcher: StringMatcher)
extends DependencyPatternNode with Dependencies {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath
  ): Seq[(Int, SynPath)] = {
    val edges = outgoingEdges(sent, doc)
    if (edges isDefinedAt tok) {
      for {
        (nextTok, label) <- edges(tok)
        if matcher.matches(label)
        newPath = (tok, nextTok, label) +: path // path is collected in reverse
      } yield (nextTok, newPath)
    } else Nil
  }
}

class IncomingDependencyPattern(matcher: StringMatcher)
extends DependencyPatternNode with Dependencies {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath
  ): Seq[(Int, SynPath)] = {
    val edges = incomingEdges(sent, doc)
    if (edges isDefinedAt tok) {
      for {
        (nextTok, label) <- edges(tok)
        if matcher.matches(label)
        newPath = (nextTok, tok, label) +: path // path is collected in reverse
      } yield (nextTok, newPath)
    } else Nil
  }
}

class ConcatDependencyPattern(lhs: DependencyPatternNode, rhs: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath
  ): Seq[(Int, SynPath)] = {
    val results = for {
      (i, p) <- lhs.findAllIn(tok, sent, doc, state, path)
      (j, q) <- rhs.findAllIn(i, sent, doc, state, p)
    } yield (j, q)
    distinct(results)
  }
}

class DisjunctiveDependencyPattern(lhs: DependencyPatternNode, rhs: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath
  ): Seq[(Int, SynPath)] = {
    val leftResults = lhs.findAllIn(tok, sent, doc, state, path)
    val rightResults = rhs.findAllIn(tok, sent, doc, state, path)
    distinct(leftResults ++ rightResults)
  }
}

class TokenConstraintDependencyPattern(constraint: TokenConstraint)
extends DependencyPatternNode {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath
  ): Seq[(Int, SynPath)] = {
    if (constraint.matches(tok, sent, doc, state)) Seq((tok, path)) else Nil
  }
}

class LookaroundDependencyPattern(lookaround: DependencyPatternNode, negative: Boolean)
extends DependencyPatternNode {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath
  ): Seq[(Int, SynPath)] = {
    val results = lookaround.findAllIn(tok, sent, doc, state)
    if (results.isEmpty == negative) Seq((tok, path)) else Nil
  }
}

class OptionalDependencyPattern(pattern: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath
  ): Seq[(Int, SynPath)] = {
    distinct((tok, path) +: pattern.findAllIn(tok, sent, doc, state, path))
  }
}

class KleeneDependencyPattern(pattern: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State,
      path: SynPath
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
        collect(rest ++ pattern.findAllIn(t, sent, doc, state, p), seen + t, (t, p) +: results)
    }
    collect(Seq((tok, path)), Set.empty, Nil)
  }
}
