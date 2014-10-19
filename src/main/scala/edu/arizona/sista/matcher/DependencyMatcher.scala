package edu.arizona.sista.matcher

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import edu.arizona.sista.processors.Sentence


case class TriggerMatcher(filterer: TokenFilter) {
  def findAllIn(sentence: Sentence): Seq[Int] =
    filterer.filter(sentence, 0 until sentence.size)
}


trait DepMatcher {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int]

  protected def dependencies(sentence: Sentence) = sentence.dependencies match {
    case None => scala.sys.error("sentence has no dependencies")
    case Some(deps) => deps
  }
}


trait StringMatcher {
  def matches(strings: Seq[(Int, String)]): Seq[Int]
}


trait TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int]

  protected def values(tokens: Seq[Int], strings: Seq[String]): Seq[(Int, String)] =
    tokens map (i => (i, strings(i)))

  protected def values(tokens: Seq[Int], strings: Option[Array[String]], msg: String): Seq[(Int, String)] =
    strings match {
      case None => scala.sys.error(msg)
      case Some(strings) => values(tokens, strings)
    }
}


case class ExactStringMatcher(dep: String) extends StringMatcher {
  def matches(strings: Seq[(Int, String)]): Seq[Int] =
    strings filter (_._2 == dep) map (_._1)
}

case class RegexStringMatcher(rx: Regex) extends StringMatcher {
  def matches(strings: Seq[(Int, String)]): Seq[Int] =
    strings filter (e => rx.findFirstIn(e._2).nonEmpty) map (_._1)
}


case class OutgoingDepMatcher(matcher: StringMatcher) extends DepMatcher {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    matcher matches dependencies(sentence).outgoingEdges(from)
}

case class IncomingDepMatcher(matcher: StringMatcher) extends DepMatcher {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    matcher matches dependencies(sentence).incomingEdges(from)
}


case class PathDepMatcher(lhs: DepMatcher, rhs: DepMatcher) extends DepMatcher {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    lhs.findAllIn(sentence, from) flatMap (i => rhs.findAllIn(sentence, i))
}


case class OrDepMatcher(lhs: DepMatcher, rhs: DepMatcher) extends DepMatcher {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    (lhs.findAllIn(sentence, from) ++ rhs.findAllIn(sentence, from)).distinct
}

case class FilteredDepMatcher(matcher: DepMatcher, filterer: TokenFilter) extends DepMatcher {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    filterer.filter(sentence, matcher.findAllIn(sentence, from))
}


case class WordFilter(matcher: StringMatcher) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.words)
}

case class LemmaFilter(matcher: StringMatcher) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.lemmas, "sentence has no lemmas")
}

case class TagFilter(matcher: StringMatcher) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.tags, "sentence has no tags")
}

case class EntityFilter(matcher: StringMatcher) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.entities, "sentence has no entities")
}

case class ChunkFilter(matcher: StringMatcher) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.chunks, "sentence has no chunks")
}

case class AndFilter(lhs: TokenFilter, rhs: TokenFilter) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    lhs.filter(sentence, tokens) intersect rhs.filter(sentence, tokens)
}

case class OrFilter(lhs: TokenFilter, rhs: TokenFilter) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    (lhs.filter(sentence, tokens) ++ rhs.filter(sentence, tokens)).distinct
}

case class NotFilter(filterer: TokenFilter) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    tokens diff filterer.filter(sentence, tokens)
}


class DependencyMatcher(val pattern: String) {
  private var triggerFieldName = "trigger"
  private var _trigger: Option[TriggerMatcher] = None
  private var _arguments: Option[Map[String, DepMatcher]] = None

  def trigger = getFieldValue(_trigger)
  def arguments = getFieldValue(_arguments)

  initialize(pattern)

  private def initialize(pattern: String) {
    val fieldPat = """(\w+)\s*:\s*(.+)""".r
    val it = fieldPat findAllIn pattern map {
      case fieldPat(name, value) => (name -> value)
    }
    val fields = Map(it.toSeq: _*)
    _trigger = Some(TriggerMatcher(Parser.parseFilter(fields(triggerFieldName))))
    _arguments = Some(fields filterKeys (_ != triggerFieldName) mapValues Parser.parseMatcher)
  }

  private def getFieldValue[T](field: Option[T]) = field match {
    case None => throw new Error("object not initialized")
    case Some(value) => value
  }

  def findAllIn(sentence: Sentence): Seq[Map[String, Seq[Int]]] = {
    trigger findAllIn sentence flatMap (i => applyRules(sentence, i))
  }

  private def applyRules(sentence: Sentence, i: Int): Option[Map[String, Seq[Int]]] = {
    val matches = arguments.keySet flatMap { name =>
      arguments(name).findAllIn(sentence, i) match {
        case Nil => None
        case indices => Some(name -> indices)
      }
    }
    if (matches.isEmpty) None
    else Some(matches.toMap)
  }

  private object Parser extends RegexParsers {
    def parseMatcher(input: String): DepMatcher = parseAll(orMatcher, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }

    def parseFilter(input: String): TokenFilter = parseAll(tokenFilter, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }

    def ident: Parser[String] =
      """\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*""".r

    def stringLiteral: Parser[String] = """"[^\\"]*(?:\\.[^\\"]*)*"""".r ^^ {
      case s =>
        def unescape(m: Regex.Match) = m.group(1) match {
          case "t" => "\t"
          case "b" => "\b"
          case "n" => "\n"
          case "r" => "\r"
          case "f" => "\f"
          case c => c
        }
        """\\(.)""".r.replaceAllIn(s.drop(1).dropRight(1), unescape _)
    }

    def exactLiteral: Parser[String] = ident | stringLiteral

    // match a perl style "/" delimited regular expression
    // "\" is the escape character, so "\/" becomes "/"
    def regexLiteral: Parser[String] = """/[^\\/]*(?:\\.[^\\/]*)*/""".r ^^ {
      case s => s.drop(1).dropRight(1).replaceAll("""\\/""", "/")
    }

    def exactMatcher: Parser[StringMatcher] = exactLiteral ^^ {
      ExactStringMatcher(_)
    }

    def regexMatcher: Parser[StringMatcher] = regexLiteral ^^ {
      case pattern => RegexStringMatcher(pattern.r)
    }

    def stringMatcher: Parser[StringMatcher] = exactMatcher | regexMatcher

    def outgoingMatcher: Parser[DepMatcher] = opt(">") ~> stringMatcher ^^ {
      OutgoingDepMatcher(_)
    }

    def incomingMatcher: Parser[DepMatcher] = "<" ~> stringMatcher ^^ {
      IncomingDepMatcher(_)
    }

    def atomMatcher: Parser[DepMatcher] =
      outgoingMatcher | incomingMatcher | "(" ~> orMatcher <~ ")"

    def filteredMatcher: Parser[DepMatcher] = atomMatcher ~ opt(tokenFilter) ^^ {
      case matcher ~ None => matcher
      case matcher ~ Some(filterer) => FilteredDepMatcher(matcher, filterer)
    }

    def pathMatcher: Parser[DepMatcher] = filteredMatcher ~ rep(filteredMatcher) ^^ {
      case first ~ rest => (first /: rest) {
        case (lhs, rhs) => PathDepMatcher(lhs, rhs)
      }
    }

    def orMatcher: Parser[DepMatcher] = pathMatcher ~ rep("|" ~> pathMatcher) ^^ {
      case first ~ rest => (first /: rest) {
        case (lhs, rhs) => OrDepMatcher(lhs, rhs)
      }
    }

    def filterName: Parser[String] = "word" | "lemma" | "tag" | "entity" | "chunk"

    def filterValue: Parser[TokenFilter] = filterName ~ "=" ~ stringMatcher ^^ {
      case "word" ~ _ ~ matcher => WordFilter(matcher)
      case "lemma" ~ _ ~ matcher => LemmaFilter(matcher)
      case "tag" ~ _ ~ matcher => TagFilter(matcher)
      case "entity" ~ _ ~ matcher => EntityFilter(matcher)
      case "chunk" ~ _ ~ matcher => ChunkFilter(matcher)
    }

    def filterAtom: Parser[TokenFilter] = filterValue | "(" ~> orFilter <~ ")"

    def notFilter: Parser[TokenFilter] = opt("!") ~ filterAtom ^^ {
      case None ~ filterer => filterer
      case Some(_) ~ filterer => NotFilter(filterer)
    }

    def andFilter: Parser[TokenFilter] = notFilter ~ rep("&" ~> notFilter) ^^ {
      case first ~ rest => (first /: rest) {
        case (lhs, rhs) => AndFilter(lhs, rhs)
      }
    }

    def orFilter: Parser[TokenFilter] = andFilter ~ rep("|" ~> andFilter) ^^ {
      case first ~ rest => (first /: rest) {
        case (lhs, rhs) => OrFilter(lhs, rhs)
      }
    }

    def tokenFilter: Parser[TokenFilter] = "[" ~> orFilter <~ "]"
  }
}
