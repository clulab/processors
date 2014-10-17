package edu.arizona.sista.matcher

import scala.util.parsing.combinator._
import edu.arizona.sista.processors.Sentence


case class TriggerMatcher(token: String) {
  def findAllIn(sentence: Sentence): Seq[Int] = {
    sentence.words.zipWithIndex filter (_._1 == token) map (_._2)
  }
}


trait Matcher {
  def findIn(sentence: Sentence, from: Int): Option[Int]

  // get dependencies for sentence if available
  protected def dependencies(sentence: Sentence) = sentence.dependencies match {
    case None => throw new Error("no dependencies in sentence")
    case Some(deps) => deps
  }
}


case class ExactOutgoingDepMatcher(dep: String) extends Matcher {
  def findIn(sentence: Sentence, from: Int): Option[Int] = {
    val deps = dependencies(sentence)
    val matches = deps.outgoingEdges(from) filter (_._2 == dep) map (_._1)
    if (matches.size == 1) Some(matches.head)
    else None
  }
}


case class ExactIncomingDepMatcher(dep: String) extends Matcher {
  def findIn(sentence: Sentence, from: Int): Option[Int] = {
    val deps = dependencies(sentence)
    val matches = deps.incomingEdges(from) filter (_._2 == dep) map (_._1)
    if (matches.size == 1) Some(matches.head)
    else None
  }
}


case class PathMatcher(lhs: Matcher, rhs: Matcher) extends Matcher {
  def findIn(sentence: Sentence, from: Int): Option[Int] = {
    lhs.findIn(sentence, from) match {
      case None => None
      case Some(i) => rhs.findIn(sentence, i)
    }
  }
}


class DependencyMatcher(val pattern: String) {
  private var triggerFieldName = "trigger"
  private var _trigger: Option[TriggerMatcher] = None
  private var _arguments: Option[Map[String, Matcher]] = None

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

  def findAllIn(sentence: Sentence): Seq[Map[String, Int]] = {
    trigger findAllIn sentence flatMap (i => applyRules(sentence, i))
  }

  def applyRules(sentence: Sentence, i: Int): Option[Map[String, Int]] = {
    val matches = arguments.keySet flatMap { name =>
      arguments(name).findIn(sentence, i) match {
        case None => None
        case Some(i) => Some(name -> i)
      }
    }
    if (matches.isEmpty) None
    else Some(matches.toMap)
  }

  private object Parser extends RegexParsers {
    def parse(input: String): Matcher = parseAll(matcher, input).get

    def token: Parser[String] = """\w+""".r

    def matcher: Parser[Matcher] = pathMatcher

    def outgoingDepMatcher: Parser[Matcher] = """>?""".r ~> token ^^ {
      ExactOutgoingDepMatcher(_)
    }

    def incomingDepMatcher: Parser[Matcher] = "<" ~> token ^^ {
      ExactIncomingDepMatcher(_)
    }

    def depMatcher: Parser[Matcher] = outgoingDepMatcher | incomingDepMatcher

    def pathMatcher: Parser[Matcher] = depMatcher ~ rep(depMatcher) ^^ {
      case m ~ rest => (m /: rest) {
        case (lhs, rhs) => PathMatcher(lhs, rhs)
      }
    }
  }
}
