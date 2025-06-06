package org.clulab.odin.impl

import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers

trait SourcingParsers extends Parsers {

  def withSource[T](name: String, parser: Parser[T]): Parser[T] = new SourcingParser(name, parser)
//  def withSource[T](name: String, parser: Parser[T]): Parser[T] = parser

  class SourcingParser[+T](name: String, parser: Parser[T]) extends Parser[T] {

    def apply(in: Input): ParseResult[T] = {
      val parseResult = parser.apply(in)

      if (parseResult.successful && parseResult.get.isInstanceOf[Sourced[_]]) {
        val start = in.offset
        val end = parseResult.next.offset
        val source = in.source.subSequence(start, end).toString
        val mappedParseResult = parseResult.get match {
          // StringMatchers
          case result: ExactStringMatcher =>
            val sourcedResult = result.copyWithSource(source)
            val sourcedParseResult = parseResult.map { (in: T) => sourcedResult }

            sourcedParseResult
          case result: RegexStringMatcher =>
            val sourcedResult = result.copyWithSource(source)
            val sourcedParseResult = parseResult.map { (in: T) => sourcedResult }

            sourcedParseResult
          // TokenPatterns
          case result: TokenPattern =>
            val sourcedResult = result.copyWithSource(source)
            val sourcedParseResult = parseResult.map { (in: T) => sourcedResult }

            sourcedParseResult
          case result: ProgramFragment => // TODO
            val sourcedResult = result.copyWithSource(source)
            val sourceParseResult = parseResult.map { (in: T) => sourcedResult }

            sourceParseResult
          // GraphPatterns
          case result: TriggerPatternGraphPattern =>
            val sourcedResult = result.copyWithSource(source)
            val sourceParseResult = parseResult.map { (in: T) => sourcedResult }

            sourceParseResult
          case result: TriggerMentionGraphPattern =>
            val sourcedResult = result.copyWithSource(source)
            val sourceParseResult = parseResult.map { (in: T) => sourcedResult }

            sourceParseResult
          case result: RelationGraphPattern =>
            val sourcedResult = result.copyWithSource(source)
            val sourceParseResult = parseResult.map { (in: T) => sourcedResult }

            sourceParseResult
          case _ => parseResult
        }

        mappedParseResult.asInstanceOf[ParseResult[T]]
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
