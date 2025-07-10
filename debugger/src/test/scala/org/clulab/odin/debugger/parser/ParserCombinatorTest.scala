package org.clulab.odin.debugger.parser

import org.clulab.utils.Test

import scala.util.Success
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object ParserCombinatorTest extends App {

  trait DebugJavaTokenParsers extends JavaTokenParsers { // RegexParsers, Parsers, depending on what is needed {
    class Wrap[+T](name: String, parser: Parser[T]) extends Parser[T] {
      def apply(in: Input): ParseResult[T] = {
        val first = in.first
        val pos = in.pos
        val offset = in.offset
        val t = parser.apply(in)
        val nextOffset = t.next.offset

        val start = offset
        val end = t.next.offset
        val string = in.source.subSequence(start, end)

        // Take the T and turn it into a debugging T which encodes the string and the start and end

        println(s"$name matches $string for token $first at position $pos offset $offset returns $t")
        t
      }
    }
  }

  // Have newWrap function differently if the debugging in on?
  // Or just all the time include this?

  // ~ sequential composition
  // rep(...) repetition
  // opt(...) optional
  class ArithParser extends DebugJavaTokenParsers {
    def expr: Parser[Any] = new Wrap("expr", term ~ rep("+" ~ term | "-" ~ term))
    def term: Parser[Any] = new Wrap("term", factor ~ rep("*" ~ factor | "/" ~ factor))
    def factor: Parser[Any] = new Wrap("factor", floatingPointNumber | "(" ~ expr ~ ")")
  }

  {
    val input = " 3 +  4 + 5"
    val parser = new ArithParser()
    val parseResult = parser.parseAll(parser.expr, input)

    val start = 0 // How can this be known for certain?
    val end = parseResult.next.offset
    val range = s"[${start + 1}.${end + 1}]"
    val source = parseResult.next.source

    println(range)
    println(source)
    println(parseResult)
  }

  class IdentifierParser extends RegexParsers {
    val ident: Parser[String] = """[a-zA-Z_]\w*""".r
  }

  {
    val input = " sth else"
    val arith = new IdentifierParser()
    val parseResult = arith.parse(arith.ident, input)

    val start = 0 // How can this be known for certain?
//    parseResult.next
//    parseResult.next.pos.line
//    parseResult.next.pos.column
    val end = parseResult.next.offset
    val range = s"[${start + 1}.${end + 1}]"
    val source = parseResult.next.source
    // Could take the result and subtract it off of next.offset to get where it started.

    println(range)
    println(source)
    println(parseResult)
  }

  class JsonParser extends JavaTokenParsers {
    def value: Parser[Any] = obj | arr | stringLiteral | floatingPointNumber | "null" | "true" | "false"
    def obj: Parser[Any] = "{" ~ repsep(member, ",") ~ "}"
    def arr: Parser[Any] = "[" ~ repsep(value, ",") ~ "]"
    def member: Parser[Any] = stringLiteral ~ ":" ~ value
  }

  {
    val input = " sth"
    val arith = new IdentifierParser()
    val parseResult = arith.parseAll(arith.ident, input)
    val start = 0 // How can this be known for certain?
    val end = parseResult.next.offset
    val range = s"[${start + 1}.${end + 1}]"
    val source = parseResult.next.source

    println(range)
    println(source)
    println(parseResult)
  }

}
