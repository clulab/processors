package edu.arizona.sista.matcher.dependencies.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import edu.arizona.sista.processors.Sentence



trait Values {
  def values(tokens: Seq[Int], strings: Seq[String]): Seq[(Int, String)] =
    tokens map (i => (i, strings(i)))

  def values(tokens: Seq[Int], strings: Option[Array[String]], msg: String): Seq[(Int, String)] =
    strings match {
      case None => scala.sys.error(msg)
      case Some(strings) => values(tokens, strings)
    }
}



trait Dependencies {
  def dependencies(sentence: Sentence) = sentence.dependencies match {
    case None => scala.sys.error("sentence has no dependencies")
    case Some(deps) => deps
  }
}



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
}

class OutgoingExtractor(matcher: Matcher) extends Extractor with Dependencies {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    matcher matches dependencies(sentence).outgoingEdges(from)
}

class IncomingExtractor(matcher: Matcher) extends Extractor with Dependencies {
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

class FilteredExtractor(matcher: Extractor, filter: Filter) extends Extractor {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] =
    filter.filter(sentence, matcher.findAllIn(sentence, from))
}



trait Filter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int]
}

class WordFilter(matcher: Matcher) extends Filter with Values {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.words)
}

class LemmaFilter(matcher: Matcher) extends Filter with Values {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.lemmas, "sentence has no lemmas")
}

class TagFilter(matcher: Matcher) extends Filter with Values {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.tags, "sentence has no tags")
}

class EntityFilter(matcher: Matcher) extends Filter with Values {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.entities, "sentence has no entities")
}

class ChunkFilter(matcher: Matcher) extends Filter with Values {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    matcher matches values(tokens, sentence.chunks, "sentence has no chunks")
}

class AndFilter(lhs: Filter, rhs: Filter) extends Filter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    lhs.filter(sentence, tokens) intersect rhs.filter(sentence, tokens)
}

class OrFilter(lhs: Filter, rhs: Filter) extends Filter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    (lhs.filter(sentence, tokens) ++ rhs.filter(sentence, tokens)).distinct
}

class NotFilter(filter: Filter) extends Filter {
  def filter(sentence: Sentence, tokens: Seq[Int]): Seq[Int] =
    tokens diff filter.filter(sentence, tokens)
}



object Parser extends RegexParsers {
  def parseExtractor(input: String): Extractor = parseAll(orExtractor, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

  def parseFilter(input: String): Filter = parseAll(tokenFilter, input) match {
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

  def outgoingExtractor: Parser[Extractor] = opt(">") ~> stringMatcher ^^ {
    new OutgoingExtractor(_)
  }

  def incomingExtractor: Parser[Extractor] = "<" ~> stringMatcher ^^ {
    new IncomingExtractor(_)
  }

  def atomExtractor: Parser[Extractor] =
    outgoingExtractor | incomingExtractor | "(" ~> orExtractor <~ ")"

  def filteredExtractor: Parser[Extractor] = atomExtractor ~ opt(tokenFilter) ^^ {
    case matcher ~ None => matcher
    case matcher ~ Some(filter) => new FilteredExtractor(matcher, filter)
  }

  def pathExtractor: Parser[Extractor] = filteredExtractor ~ rep(filteredExtractor) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new PathExtractor(lhs, rhs)
    }
  }

  def orExtractor: Parser[Extractor] = pathExtractor ~ rep("|" ~> pathExtractor) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new OrExtractor(lhs, rhs)
    }
  }

  def filterName: Parser[String] = "word" | "lemma" | "tag" | "entity" | "chunk"

  def filterValue: Parser[Filter] = filterName ~ "=" ~ stringMatcher ^^ {
    case "word" ~ _ ~ matcher => new WordFilter(matcher)
    case "lemma" ~ _ ~ matcher => new LemmaFilter(matcher)
    case "tag" ~ _ ~ matcher => new TagFilter(matcher)
    case "entity" ~ _ ~ matcher => new EntityFilter(matcher)
    case "chunk" ~ _ ~ matcher => new ChunkFilter(matcher)
  }

  def filterAtom: Parser[Filter] = filterValue | "(" ~> orFilter <~ ")"

  def notFilter: Parser[Filter] = opt("!") ~ filterAtom ^^ {
    case None ~ filter => filter
    case Some(_) ~ filter => new NotFilter(filter)
  }

  def andFilter: Parser[Filter] = notFilter ~ rep("&" ~> notFilter) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new AndFilter(lhs, rhs)
    }
  }

  def orFilter: Parser[Filter] = andFilter ~ rep("|" ~> andFilter) ^^ {
    case first ~ rest => (first /: rest) {
      case (lhs, rhs) => new OrFilter(lhs, rhs)
    }
  }

  def tokenFilter: Parser[Filter] = "[" ~> orFilter <~ "]"
}
