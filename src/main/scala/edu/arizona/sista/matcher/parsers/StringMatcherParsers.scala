package edu.arizona.sista.matcher

import scala.util.matching.Regex
import scala.util.parsing.combinator._

trait StringMatcherParsers extends RegexParsers {
  // positive integer
  def int: Parser[Int] = """\d+""".r ^^ { _.toInt }

  // valid java identifier
  def ident: Parser[String] =
    """\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*""".r

  // single- or double-quote delimited string literal
  // with "\" as the escape character
  def quotedLiteral: Parser[String] =
    """'[^\\']*(?:\\.[^\\']*)*'|"[^\\"]*(?:\\.[^\\"]*)*"""".r ^^ {
      case s => """\\(.)""".r.replaceAllIn(s.drop(1).dropRight(1), m => m.group(1))
    }

  // any valid string literal (with or without quotes)
  def stringLiteral: Parser[String] = ident | quotedLiteral

  // match a perl style "/" delimited regular expression
  // "\" is the escape character, so "\/" becomes "/"
  def regexLiteral: Parser[Regex] = """/[^\\/]*(?:\\.[^\\/]*)*/""".r ^^ {
    case s => s.drop(1).dropRight(1).replaceAll("""\\/""", "/").r
  }

  // a StringMatcher that compares to a string
  def exactStringMatcher: Parser[StringMatcher] = stringLiteral ^^ {
    case string => new ExactStringMatcher(string)
  }

  // a StringMatcher that uses a regex
  def regexStringMatcher: Parser[StringMatcher] = regexLiteral ^^ {
    case regex => new RegexStringMatcher(regex)
  }

  // any valid StringMatcher
  def stringMatcher: Parser[StringMatcher] = exactStringMatcher | regexStringMatcher
}

sealed trait StringMatcher {
  def matches(s: String): Boolean
  def filter(strings: Seq[(Int, String)]): Seq[Int] =
    strings filter (s => matches(s._2)) map (_._1)
}

class ExactStringMatcher(string: String) extends StringMatcher {
  def matches(s: String): Boolean = s == string
}

class RegexStringMatcher(regex: Regex) extends StringMatcher {
  def matches(s: String): Boolean = regex.findFirstIn(s).nonEmpty
}
