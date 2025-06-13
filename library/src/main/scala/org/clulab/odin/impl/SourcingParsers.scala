package org.clulab.odin.impl

import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers

trait SourcingParsers extends Parsers {

  def withSource[T](name: String, parser: Parser[T]): Parser[T] = new SourcingParser(name, parser)
//  def withSource[T](name: String, parser: Parser[T]): Parser[T] = parser

  class SourcingParser[+T](name: String, parser: Parser[T]) extends Parser[T] {

    def apply(in: Input): ParseResult[T] = {
      val parseResult = parser.apply(in)

      def copy(parseResult: ParseResult[T], source: String): ParseResult[T] = {
        val result = parseResult.get.asInstanceOf[Sourced[T]]
        val sourcedResult = result.copyWithSource(source)
        val sourcedParseResult = parseResult.map { (in: T) => sourcedResult }

        sourcedParseResult
      }

      if (parseResult.successful && parseResult.get.isInstanceOf[Sourced[_]]) {
        val start = in.offset
        val end = parseResult.next.offset
        val source = in.source.subSequence(start, end).toString
        val sourcedParseResult = copy(parseResult, source)

        sourcedParseResult
      }
      else parseResult
    }

    override def ^^[U](f: T => U): Parser[U] = {
      val result = super.^^(f)

      new SourcingParser(s"$name ^^", result)
    }
  }
  // Maybe need a variation of this that takes a sequence of some kind.
}
