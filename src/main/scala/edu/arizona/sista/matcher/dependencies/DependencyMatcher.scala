package edu.arizona.sista.matcher.dependencies

import edu.arizona.sista.processors.Sentence

class DependencyMatcher(val pattern: String) {
  private var triggerFieldName = "trigger"
  private var _trigger: Option[TriggerMatcher] = None
  private var _arguments: Option[Map[String, Extractor]] = None

  def trigger = getFieldValue(_trigger)
  def arguments = getFieldValue(_arguments)

  initialize(pattern)

  private def initialize(pattern: String) {
    val fieldPat = """(\w+)\s*:\s*(.+)""".r
    val it = fieldPat findAllIn pattern map {
      case fieldPat(name, value) => (name -> value)
    }
    val fields = Map(it.toSeq: _*)
    _trigger = Some(new TriggerMatcher(Parser.parseFilter(fields(triggerFieldName))))
    _arguments = Some(fields filterKeys (_ != triggerFieldName) mapValues Parser.parseMatcher)
  }

  private def getFieldValue[T](field: Option[T]) = field match {
    case None => scala.sys.error("object not initialized")
    case Some(value) => value
  }

  def findAllIn(sentence: Sentence): Seq[Map[String, Seq[Int]]] = {
    trigger findAllIn sentence flatMap (i => applyRules(sentence, i))
  }

  private def applyRules(sentence: Sentence, i: Int): Option[Map[String, Seq[Int]]] = {
    val matches = arguments.keySet flatMap { name =>
      arguments(name).findAllIn(sentence, i) match {
        case Nil => None
        case indices => Some(name -> indices)
      }
    }
    if (matches.isEmpty) None
    else Some(matches.toMap)
  }
}

object DependencyMatcher {
  def apply(pattern: String) = new DependencyMatcher(pattern)
}

class TriggerMatcher(filterer: Filterer) {
  def findAllIn(sentence: Sentence): Seq[Int] =
    filterer.filter(sentence, 0 until sentence.size)
}
