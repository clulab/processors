package edu.arizona.sista.matcher

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import edu.arizona.sista.processors.Sentence


case class TriggerMatcher(token: String) {
  def findAllIn(sentence: Sentence): Seq[Int] = {
    sentence.words.zipWithIndex filter (_._1 == token) map (_._2)
  }
}


trait DepMatcher {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int]
}


trait NameMatcher {
  def matches(edges: Seq[(Int, String)]): Seq[Int]
}


case class ExactNameMatcher(dep: String) extends NameMatcher {
  def matches(edges: Seq[(Int, String)]): Seq[Int] = {
    edges filter (_._2 == dep) map (_._1)
  }
}

case class RegexNameMatcher(rx: Regex) extends NameMatcher {
  def matches(edges: Seq[(Int, String)]): Seq[Int] = {
    edges filter (e => rx.findFirstIn(e._2).nonEmpty) map (_._1)
  }
}


object Direction extends Enumeration {
  type Direction = Value
  val Incoming, Outgoing = Value
}
import Direction._


case class DirectedDepMatcher(matcher: NameMatcher, direction: Direction) extends DepMatcher {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] = {
    val deps = sentence.dependencies match {
      case None => throw new Error("sentence has no dependencies")
      case Some(deps) => deps
    }
    val edges = if (direction == Incoming) deps.incomingEdges else deps.outgoingEdges
    matcher matches edges(from)
  }
}


case class PathDepMatcher(lhs: DepMatcher, rhs: DepMatcher) extends DepMatcher {
  def findAllIn(sentence: Sentence, from: Int): Seq[Int] = {
    lhs.findAllIn(sentence, from) flatMap (i => rhs.findAllIn(sentence, i))
  }
}


class DependencyMatcher(val pattern: String) {
  private var triggerFieldName = "trigger"
  private var _trigger: Option[TriggerMatcher] = None
  private var _arguments: Option[Map[String, DepMatcher]] = None

  def trigger = getFieldValue(_trigger)

  def arguments = getFieldValue(_arguments)

  parse(pattern)

  private def parse(pattern: String) {
    val fieldPat = """(\w+)\s*:\s*(.+)""".r
    val it = fieldPat findAllIn pattern map {
      case fieldPat(name, value) => (name -> value)
    }
    val fields = Map(it.toSeq: _*)
    _trigger = Some(TriggerMatcher(fields(triggerFieldName)))
    _arguments = Some(fields filterKeys (_ != triggerFieldName) mapValues Parser.parse)
  }

  private def getFieldValue[T](field: Option[T]) = field match {
    case None => throw new Error("object not initialized")
    case Some(value) => value
  }

  def findAllIn(sentence: Sentence): Seq[Map[String, Seq[Int]]] = {
    trigger findAllIn sentence flatMap (i => applyRules(sentence, i))
  }

  def applyRules(sentence: Sentence, i: Int): Option[Map[String, Seq[Int]]] = {
    val matches = arguments.keySet flatMap { name =>
      arguments(name).findAllIn(sentence, i) match {
        case Nil => None
        case indices => Some(name -> indices)
      }
    }
    if (matches.isEmpty) None
    else Some(matches.toMap)
  }

  private object Parser extends JavaTokenParsers {
    def parse(input: String): DepMatcher = parseAll(matcher, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }

    def matcher: Parser[DepMatcher] = pathMatcher

    def exactMatcher: Parser[NameMatcher] = ident ^^ {
      ExactNameMatcher(_)
    }

    // match a perl style "/" delimited regular expression
    // "\" is the escape character, so "\/" becomes "/"
    def regexLiteral: Parser[String] = """/([^\\/]*(?:\\.[^\\/]*)*)/""".r ^^ {
      case s => s.drop(1).dropRight(1).replaceAll("""\\/""", "/")
    }

    def regexMatcher: Parser[NameMatcher] = regexLiteral ^^ {
      case pattern => RegexNameMatcher(pattern.r)
    }

    def nameMatcher: Parser[NameMatcher] = exactMatcher | regexMatcher

    def outgoingMatcher: Parser[DepMatcher] = opt(">") ~> nameMatcher ^^ {
      DirectedDepMatcher(_, Outgoing)
    }

    def incomingMatcher: Parser[DepMatcher] = "<" ~> nameMatcher ^^ {
      DirectedDepMatcher(_, Incoming)
    }

    def depMatcher: Parser[DepMatcher] = outgoingMatcher | incomingMatcher

    def pathMatcher: Parser[DepMatcher] = depMatcher ~ rep(depMatcher) ^^ {
      case m ~ rest => (m /: rest) {
        case (lhs, rhs) => PathDepMatcher(lhs, rhs)
      }
    }
  }
}
