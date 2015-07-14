package edu.arizona.sista.odin.impl

import java.util.{ Collection, Map => JMap }
import scala.reflect.ClassTag
import scala.beans.BeanProperty
import scala.collection.JavaConverters._
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor
import edu.arizona.sista.odin._
import RuleReader._

class RuleReader[A <: Actions : ClassTag](val actions: A) {
  // invokes actions through reflection
  private val mirror = new ActionMirror(actions)

  def read(input: String): Seq[Extractor] = {
    // read yaml rules
    val rules = readRules(input)
    // count names occurrences
    val names = rules groupBy (_.name) transform ((k, v) => v.size)
    // names should be unique
    names find (_._2 > 1) match {
      case None => rules map mkExtractor  // return extractors
      case Some((name, count)) => throw OdinCompileException(s"rule name '$name' is not unique", Some(name))
    }
  }

  private def readRules(input: String): Seq[Rule] = {
    // make yaml object
    val yaml = new Yaml(new Constructor(classOf[Collection[JMap[String, Any]]]))

    // parse yaml input
    val rules = yaml.load(input).asInstanceOf[Collection[JMap[String, Any]]]

    // return Rule objects
    rules.asScala.toSeq.map { r =>
      val m = r.asScala.toMap

      // name is required
      val name = try {
        m("name").toString()
      } catch {
        case e: Exception => throw OdinCompileException("unnamed rule")
      }

      // one or more labels are required
      val labels: Seq[String] = try {
        m("label") match {
          case label: String => Seq(label)
          case labels: Collection[_] => labels.asScala.map(_.toString).toSeq.distinct
        }
      } catch {
        case e: Exception => throw OdinCompileException(s"rule '$name' has no labels", Some(name))
      }

      // pattern is required
      val pattern = try {
        m("pattern").toString()
      } catch {
        case e: Exception => throw OdinCompileException(s"rule '$name' has no pattern", Some(name))
      }

      // these fields have default values
      val ruleType = m.getOrElse("type", DefaultType).toString()
      val priority = m.getOrElse("priority", DefaultPriority).toString()
      val action = m.getOrElse("action", DefaultAction).toString()
      val keep = m.getOrElse("keep", DefaultKeep).asInstanceOf[Boolean]
      // unit is relevant to TokenPattern only
      val unit = m.getOrElse("unit", DefaultUnit).toString()

      // make intermediary rule
      new Rule(name, labels, ruleType, unit, priority, keep, action, pattern)
    }
  }

  // compiles a rule into an extractor
  private def mkExtractor(rule: Rule): Extractor = {
    try {
      rule.ruleType match {
        case "token" => mkTokenExtractor(rule)
        case "dependency" => mkDependencyExtractor(rule)
        case _ => 
          throw OdinCompileException(s"rule '${rule.name}' has unsupported type '${rule.ruleType}'", Some(rule.name))
      }
    } catch {
      case e: Exception => throw OdinCompileException(s"Error parsing rule '${rule.name}': ${e.getMessage}", Some(rule.name))
    }
  }

  // compiles a token extractor
  private def mkTokenExtractor(rule: Rule): TokenExtractor = {
    val name = rule.name
    val labels = rule.labels
    val priority = Priority(rule.priority)
    val keep = rule.keep
    val action = mirror.reflect(rule.action)
    val compiler = new TokenPatternParsers(rule.unit)
    val pattern = compiler.compileTokenPattern(rule.pattern)
    new TokenExtractor(name, labels, priority, keep, action, pattern)
  }

  // compiles a dependency extractor
  private def mkDependencyExtractor(rule: Rule): DependencyExtractor = {
    val name = rule.name
    val labels = rule.labels
    val priority = Priority(rule.priority)
    val keep = rule.keep
    val action = mirror.reflect(rule.action)
    val compiler = new DependencyPatternCompiler(rule.unit)
    val pattern = compiler.compileDependencyPattern(rule.pattern)
    new DependencyExtractor(name, labels, priority, keep, action, pattern)
  }
}

object RuleReader {
  val DefaultType = "dependency"
  val DefaultPriority = "1+"
  val DefaultKeep = true
  val DefaultAction = "default"
  val DefaultUnit = "word"

  // rule intermediary representation
  class Rule(
    val name: String,
    val labels: Seq[String],
    val ruleType: String,
    val unit: String,
    val priority: String,
    val keep: Boolean,
    val action: String,
    val pattern: String
  )
}
