package edu.arizona.sista.matcher

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document

class DependencyPattern(val trigger: TokenPattern, val arguments: Seq[ArgumentPattern]) {
  // the labels of the required arguments
  private val required = for (a <- arguments if a.required) yield a.name

  type Match = Map[String, Seq[Interval]]

  // returns matches with sentence id
  def findAllIn(doc: Document): Seq[(Match, Int)] = for {
    i <- 0 until doc.sentences.size
    m <- findAllIn(i, doc)
  } yield (m, i)

  def findAllIn(sent: Int, doc: Document): Seq[Match] =
    for {
      r <- trigger.findAllIn(sent, doc)
      trig = Interval(r.start, r.end)
      args <- extractArguments(trig, sent, doc)
    } yield args + ("trigger" -> Seq(trig))

  // extract the arguments of a trigger represented as a token interval
  private def extractArguments(trig: Interval, sent: Int, doc: Document): Option[Match] = {
    val matches = trig.toRange.map(tok => extractArguments(tok, sent, doc))
    if (matches.isEmpty) None
    else {
      val result = matches reduce mergeMatches
      if (required exists (arg => result.getOrElse(arg, Nil).isEmpty)) None
      else Some(result)
    }
  }

  // extract arguments for one of the tokens in the trigger
  private def extractArguments(tok: Int, sent: Int, doc: Document): Match =
    arguments.map(a => (a.name -> a.findAllIn(tok, sent, doc))).toMap

  private def mergeMatches(lhs: Match, rhs: Match): Match = {
    val keys = lhs.keySet ++ rhs.keySet
    val args = keys map (k => (k -> (lhs.getOrElse(k, Nil) ++ rhs.getOrElse(k, Nil))))
    args.toMap
  }
}

object DependencyPattern {
  def compile(input: String): DependencyPattern = DependencyPatternCompiler.compile(input)
}

object DependencyPatternCompiler extends TokenPatternParsers {
  def compile(input: String): DependencyPattern = parseAll(dependencyPattern, input.trim) match {
    case Success(result, _) => result
    case failure: NoSuccess => sys.error(failure.msg)
  }

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
  def findAllIn(tok: Int, sent: Int, doc: Document): Seq[Interval] =
    pattern.findAllIn(tok, sent, doc) map Interval.apply
}

sealed trait DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document): Seq[Int]
}

class OutgoingDependencyPattern(matcher: StringMatcher)
extends DependencyPatternNode with Dependencies {
  def findAllIn(tok: Int, sent: Int, doc: Document): Seq[Int] = {
    val edges = outgoingEdges(sent, doc)
    if (edges isDefinedAt tok) matcher.filter(edges(tok)) else Nil
  }
}

class IncomingDependencyPattern(matcher: StringMatcher)
extends DependencyPatternNode with Dependencies {
  def findAllIn(tok: Int, sent: Int, doc: Document): Seq[Int] = {
    val edges = incomingEdges(sent, doc)
    if (edges isDefinedAt tok) matcher.filter(edges(tok)) else Nil
  }
}

class ConcatDependencyPattern(lhs: DependencyPatternNode, rhs: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document): Seq[Int] =
    (lhs.findAllIn(tok, sent, doc) flatMap (i => rhs.findAllIn(i, sent, doc))).distinct
}

class DisjunctiveDependencyPattern(lhs: DependencyPatternNode, rhs: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document): Seq[Int] =
    (lhs.findAllIn(tok, sent, doc) ++ rhs.findAllIn(tok, sent, doc)).distinct
}

class FilteredDependencyPattern(pattern: DependencyPatternNode, constraint: TokenConstraint)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document): Seq[Int] = {
    val tokens = pattern.findAllIn(tok, sent, doc)
    constraint.filter(tokens, sent, doc)
  }
}

class OptionalDependencyPattern(pattern: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document): Seq[Int] =
    (tok +: pattern.findAllIn(tok, sent, doc)).distinct
}

class KleeneDependencyPattern(pattern: DependencyPatternNode)
extends DependencyPatternNode {
  def findAllIn(tok: Int, sent: Int, doc: Document): Seq[Int] = {
    @annotation.tailrec
    def loop(remaining: Seq[Int], results: Seq[Int]): Seq[Int] = remaining match {
      case Nil => results
      case t :: ts if results contains t => loop(ts, results)
      case t :: ts => loop(ts ++ pattern.findAllIn(t, sent, doc), t +: results)
    }
    loop(Seq(tok), Nil)
  }
}
