package edu.arizona.sista.matcher.dependencies

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import edu.arizona.sista.processors.Sentence



trait Matcher {
  def matches(strings: Seq[(Int, String)]): Seq[Int]
}

class ExactMatcher(dep: String) extends Matcher {
  def matches(strings: Seq[(Int, String)]): Seq[Int] =
    strings filter (_._2 == dep) map (_._1)
}

class RegexMatcher(rx: Regex) extends Matcher {
  def matches(strings: Seq[(Int, String)]): Seq[Int] =
    strings filter (e => rx.findFirstIn(e._2).nonEmpty) map (_._1)
}



trait Extractor {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int]

  protected def dependencies(sentence: Sentence) = sentence.dependencies match {
    case None => scala.sys.error("sentence has no dependencies")
    case Some(deps) => deps
  }
}

class OutgoingExtractor(matcher: Matcher) extends Extractor {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    matcher matches dependencies(sentence).outgoingEdges(from)
}

class IncomingExtractor(matcher: Matcher) extends Extractor {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    matcher matches dependencies(sentence).incomingEdges(from)
}

class PathExtractor(lhs: Extractor, rhs: Extractor) extends Extractor {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    lhs.findAllIn(sentence, from) flatMap (i => rhs.findAllIn(sentence, i))
}

class OrExtractor(lhs: Extractor, rhs: Extractor) extends Extractor {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    (lhs.findAllIn(sentence, from) ++ rhs.findAllIn(sentence, from)).distinct
}

class FilteredExtractor(matcher: Extractor, filterer: Filterer) extends Extractor {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    filterer.filter(sentence, matcher.findAllIn(sentence, from))
}



trait Filterer {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int]

  protected def values(tokens: Seq[Int], strings: Seq[String]): Seq[(Int, String)] =
    tokens map (i => (i, strings(i)))

  protected def values(tokens: Seq[Int], strings: Option[Array[String]], msg: String): Seq[(Int, String)] =
    strings match {
      case None => scala.sys.error(msg)
      case Some(strings) => values(tokens, strings)
    }
}

class WordFilter(matcher: Matcher) extends Filterer {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.words)
}

class LemmaFilter(matcher: Matcher) extends Filterer {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.lemmas, "sentence has no lemmas")
}

class TagFilter(matcher: Matcher) extends Filterer {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.tags, "sentence has no tags")
}

class EntityFilter(matcher: Matcher) extends Filterer {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.entities, "sentence has no entities")
}

class ChunkFilter(matcher: Matcher) extends Filterer {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.chunks, "sentence has no chunks")
}

class AndFilter(lhs: Filterer, rhs: Filterer) extends Filterer {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    lhs.filter(sentence, tokens) intersect rhs.filter(sentence, tokens)
}

class OrFilter(lhs: Filterer, rhs: Filterer) extends Filterer {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    (lhs.filter(sentence, tokens) ++ rhs.filter(sentence, tokens)).distinct
}

class NotFilter(filterer: Filterer) extends Filterer {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    tokens diff filterer.filter(sentence, tokens)
}



object Parser extends RegexParsers {
  def parseMatcher(input: String): Extractor = parseAll(orMatcher, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

  def parseFilter(input: String): Filterer = parseAll(tokenFilter, input) match {
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

  def exactMatcher: Parser[Matcher] = exactLiteral ^^ {
    new ExactMatcher(_)
  }

  def regexMatcher: Parser[Matcher] = regexLiteral ^^ {
    case pattern => new RegexMatcher(pattern.r)
  }

  def stringMatcher: Parser[Matcher] = exactMatcher | regexMatcher

  def outgoingMatcher: Parser[Extractor] = opt(">") ~> stringMatcher ^^ {
    new OutgoingExtractor(_)
  }

  def incomingMatcher: Parser[Extractor] = "<" ~> stringMatcher ^^ {
    new IncomingExtractor(_)
  }

  def atomMatcher: Parser[Extractor] =
    outgoingMatcher | incomingMatcher | "(" ~> orMatcher <~ ")"

  def filteredMatcher: Parser[Extractor] = atomMatcher ~ opt(tokenFilter) ^^ {
    case matcher ~ None => matcher
    case matcher ~ Some(filterer) => new FilteredExtractor(matcher, filterer)
  }

  def pathMatcher: Parser[Extractor] = filteredMatcher ~ rep(filteredMatcher) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new PathExtractor(lhs, rhs)
    }
  }

  def orMatcher: Parser[Extractor] = pathMatcher ~ rep("|" ~> pathMatcher) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new OrExtractor(lhs, rhs)
    }
  }

  def filterName: Parser[String] = "word" | "lemma" | "tag" | "entity" | "chunk"

  def filterValue: Parser[Filterer] = filterName ~ "=" ~ stringMatcher ^^ {
    case "word" ~ _ ~ matcher => new WordFilter(matcher)
    case "lemma" ~ _ ~ matcher => new LemmaFilter(matcher)
    case "tag" ~ _ ~ matcher => new TagFilter(matcher)
    case "entity" ~ _ ~ matcher => new EntityFilter(matcher)
    case "chunk" ~ _ ~ matcher => new ChunkFilter(matcher)
  }

  def filterAtom: Parser[Filterer] = filterValue | "(" ~> orFilter <~ ")"

  def notFilter: Parser[Filterer] = opt("!") ~ filterAtom ^^ {
    case None ~ filterer => filterer
    case Some(_) ~ filterer => new NotFilter(filterer)
  }

  def andFilter: Parser[Filterer] = notFilter ~ rep("&" ~> notFilter) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new AndFilter(lhs, rhs)
    }
  }

  def orFilter: Parser[Filterer] = andFilter ~ rep("|" ~> andFilter) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new OrFilter(lhs, rhs)
    }
  }

  def tokenFilter: Parser[Filterer] = "[" ~> orFilter <~ "]"
}
