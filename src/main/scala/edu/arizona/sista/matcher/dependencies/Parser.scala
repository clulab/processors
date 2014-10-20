package edu.arizona.sista.matcher.dependencies

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import edu.arizona.sista.processors.Sentence



trait StringMatcher {
  def matches(strings: Seq[(Int, String)]): Seq[Int]
}

class ExactStringMatcher(dep: String) extends StringMatcher {
  def matches(strings: Seq[(Int, String)]): Seq[Int] =
    strings filter (_._2 == dep) map (_._1)
}

class RegexStringMatcher(rx: Regex) extends StringMatcher {
  def matches(strings: Seq[(Int, String)]): Seq[Int] =
    strings filter (e => rx.findFirstIn(e._2).nonEmpty) map (_._1)
}



trait DepMatcher {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int]

  protected def dependencies(sentence: Sentence) = sentence.dependencies match {
    case None => scala.sys.error("sentence has no dependencies")
    case Some(deps) => deps
  }
}

class OutgoingDepMatcher(matcher: StringMatcher) extends DepMatcher {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    matcher matches dependencies(sentence).outgoingEdges(from)
}

class IncomingDepMatcher(matcher: StringMatcher) extends DepMatcher {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    matcher matches dependencies(sentence).incomingEdges(from)
}

class PathDepMatcher(lhs: DepMatcher, rhs: DepMatcher) extends DepMatcher {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    lhs.findAllIn(sentence, from) flatMap (i => rhs.findAllIn(sentence, i))
}

class OrDepMatcher(lhs: DepMatcher, rhs: DepMatcher) extends DepMatcher {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    (lhs.findAllIn(sentence, from) ++ rhs.findAllIn(sentence, from)).distinct
}

class FilteredDepMatcher(matcher: DepMatcher, filterer: TokenFilter) extends DepMatcher {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    filterer.filter(sentence, matcher.findAllIn(sentence, from))
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

class WordFilter(matcher: StringMatcher) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.words)
}

class LemmaFilter(matcher: StringMatcher) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.lemmas, "sentence has no lemmas")
}

class TagFilter(matcher: StringMatcher) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.tags, "sentence has no tags")
}

class EntityFilter(matcher: StringMatcher) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.entities, "sentence has no entities")
}

class ChunkFilter(matcher: StringMatcher) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.chunks, "sentence has no chunks")
}

class AndFilter(lhs: TokenFilter, rhs: TokenFilter) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    lhs.filter(sentence, tokens) intersect rhs.filter(sentence, tokens)
}

class OrFilter(lhs: TokenFilter, rhs: TokenFilter) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    (lhs.filter(sentence, tokens) ++ rhs.filter(sentence, tokens)).distinct
}

class NotFilter(filterer: TokenFilter) extends TokenFilter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    tokens diff filterer.filter(sentence, tokens)
}



object Parser extends RegexParsers {
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

  // single- or double-quote delimited string literal
  def stringLiteral: Parser[String] =
    """"[^\\"]*(?:\\.[^\\"]*)*"|'[^\\']*(?:\\.[^\\']*)*'""".r ^^ {
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
    new ExactStringMatcher(_)
  }

  def regexMatcher: Parser[StringMatcher] = regexLiteral ^^ {
    case pattern => new RegexStringMatcher(pattern.r)
  }

  def stringMatcher: Parser[StringMatcher] = exactMatcher | regexMatcher

  def outgoingMatcher: Parser[DepMatcher] = opt(">") ~> stringMatcher ^^ {
    new OutgoingDepMatcher(_)
  }

  def incomingMatcher: Parser[DepMatcher] = "<" ~> stringMatcher ^^ {
    new IncomingDepMatcher(_)
  }

  def atomMatcher: Parser[DepMatcher] =
    outgoingMatcher | incomingMatcher | "(" ~> orMatcher <~ ")"

  def filteredMatcher: Parser[DepMatcher] = atomMatcher ~ opt(tokenFilter) ^^ {
    case matcher ~ None => matcher
    case matcher ~ Some(filterer) => new FilteredDepMatcher(matcher, filterer)
  }

  def pathMatcher: Parser[DepMatcher] = filteredMatcher ~ rep(filteredMatcher) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new PathDepMatcher(lhs, rhs)
    }
  }

  def orMatcher: Parser[DepMatcher] = pathMatcher ~ rep("|" ~> pathMatcher) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new OrDepMatcher(lhs, rhs)
    }
  }

  def filterName: Parser[String] = "word" | "lemma" | "tag" | "entity" | "chunk"

  def filterValue: Parser[TokenFilter] = filterName ~ "=" ~ stringMatcher ^^ {
    case "word" ~ _ ~ matcher => new WordFilter(matcher)
    case "lemma" ~ _ ~ matcher => new LemmaFilter(matcher)
    case "tag" ~ _ ~ matcher => new TagFilter(matcher)
    case "entity" ~ _ ~ matcher => new EntityFilter(matcher)
    case "chunk" ~ _ ~ matcher => new ChunkFilter(matcher)
  }

  def filterAtom: Parser[TokenFilter] = filterValue | "(" ~> orFilter <~ ")"

  def notFilter: Parser[TokenFilter] = opt("!") ~ filterAtom ^^ {
    case None ~ filterer => filterer
    case Some(_) ~ filterer => new NotFilter(filterer)
  }

  def andFilter: Parser[TokenFilter] = notFilter ~ rep("&" ~> notFilter) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new AndFilter(lhs, rhs)
    }
  }

  def orFilter: Parser[TokenFilter] = andFilter ~ rep("|" ~> andFilter) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new OrFilter(lhs, rhs)
    }
  }

  def tokenFilter: Parser[TokenFilter] = "[" ~> orFilter <~ "]"
}
