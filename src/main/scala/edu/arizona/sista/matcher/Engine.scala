package edu.arizona.sista.matcher

import scala.collection.mutable.HashMap

trait RuleMatcher

class SpecMatcher(val spec: String) {
  private var _name: Option[String] = None
  def name = getFieldValue(_name)

  private var _priority: Option[Int] = None
  def priority = getFieldValue(_priority)

  private var _ruleType: Option[String] = None
  def ruleType = getFieldValue(_ruleType)

  private var _pattern: Option[String] = None
  def pattern = getFieldValue(_pattern)

  // initialize SpecMatcher objects
  parse(spec)

  private def parse(spec: String) {
    val field = """(?s)(\w+)\s*:\s*(\w+|\{\{.*\}\})""".r
    val it = for (field(name, value) <- field findAllIn spec) yield (name -> value)
    val fields = Map(it.toSeq: _*)
    _name = Some(fields("name"))
    _priority = Some(fields("priority").toInt)
    _ruleType = Some(fields("type"))
    _pattern = Some(fields("pattern").drop(2).dropRight(2))
  }

  private def getFieldValue[T](field: Option[T]) = field match {
    case None => throw new Error("object not initialized")
    case Some(value) => value
  }
}

object SpecMatcher {
  type RuleMatcherConstructor = String => RuleMatcher

  private val registeredMatchers = new HashMap[String, RuleMatcherConstructor]

  def register(name: String, func: RuleMatcherConstructor) {
    registeredMatchers += (name -> func)
  }

  def mkRuleMatcher(name: String, pattern: String): RuleMatcher =
    registeredMatchers(name)(pattern)
}
