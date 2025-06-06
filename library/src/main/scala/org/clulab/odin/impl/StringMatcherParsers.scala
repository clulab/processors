package org.clulab.odin.impl

import scala.language.reflectiveCalls
import scala.util.matching.Regex
import scala.util.parsing.combinator._

trait StringMatcherParsers extends RegexParsers with SourcingParsers {

  // any valid StringMatcher
  def stringMatcher: Parser[StringMatcher] = exactStringMatcher | regexStringMatcher

  // a StringMatcher that compares to a string
  def exactStringMatcher: Parser[ExactStringMatcher] = {
    val parser1 = stringLiteral
    val parser2 = withSource("exactStringMatcher", parser1)
    val parser3 = parser2 ^^ {
      string => new ExactStringMatcher(string)
    }

    parser3
  }

  // a StringMatcher that uses a regex
  def regexStringMatcher: Parser[RegexStringMatcher] = {
    val parser1 = regexLiteral
    val parser2 = withSource("regexStringMatcher", parser1)
    val parser3 = parser2 ^^ {
      regex => new RegexStringMatcher(regex)
    }

    parser3
  }

  // any valid string literal (with or without quotes)
  def stringLiteral: Parser[String] = odinIdentifier | quotedStringLiteral

  // valid java identifier
  def javaIdentifier: Parser[String] =
    """\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*""".r

  // identifier that can also contain colons and dashes except in the first and last characters
  def odinIdentifier: Parser[String] =
    """\p{javaJavaIdentifierStart}(?:[-:\p{javaJavaIdentifierPart}]*\p{javaJavaIdentifierPart})?""".r

  // single- or double-quote delimited string literal
  // with "\" as the escape character
  def quotedStringLiteral: Parser[String] =
    """'[^\\']*(?:\\.[^\\']*)*'|"[^\\"]*(?:\\.[^\\"]*)*"""".r ^^ {
      s => """\\(.)""".r.replaceAllIn(s.drop(1).dropRight(1), m => m.group(1))
    }

  // match a perl style "/" delimited regular expression
  // "\" is the escape character, so "\/" becomes "/"
  def regexLiteral: Parser[Regex] = """/[^\\/]*(?:\\.[^\\/]*)*/""".r ^^ {
    s => s.drop(1).dropRight(1).replaceAll("""\\/""", "/").r
  }

}

sealed trait StringMatcher extends Sourced[StringMatcher] {
  def matches(s: String): Boolean
}

class ExactStringMatcher(val string: String, val sourceOpt: Option[String] = None) extends StringMatcher {
  def matches(s: String): Boolean = string == s

  override def copyWithSource(source: String): StringMatcher =
      new ExactStringMatcher(string, Some(source))
}

class RegexStringMatcher(val regex: Regex, val sourceOpt: Option[String] = None) extends StringMatcher {
  def matches(s: String): Boolean = regex.findFirstIn(s).nonEmpty

  override def copyWithSource(source: String): StringMatcher =
      new RegexStringMatcher(regex, Some(source))
}
