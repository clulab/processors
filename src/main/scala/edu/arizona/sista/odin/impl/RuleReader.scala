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

  // rule intermediary representation
  class Rule(
    val name: String,
    val labels: Set[String],
    val ruleType: String,
    val priority: String,
    val keep: Boolean,
    val action: String,
    val pattern: String
  )

  def read(input: String): Seq[Extractor] = {
    // read yaml rules
    val rules = readRules(input)
    // count names occurrences
    val names = rules groupBy (_.name) mapValues (_.size)
    // names should be unique
    names find (_._2 > 1) match {
      case None => rules map mkExtractor  // return extractors
      case Some((name, count)) => sys.error(s"rule name '$name' is not unique")
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
        case e: Exception => sys.error("unnamed rule")
      }

      // one or more labels are required
      val labels: Set[String] = try {
        m("label") match {
          case label: String => Set(label)
          case labels: Collection[_] => labels.asScala.map(_.toString).toSet
        }
      } catch {
        case e: Exception => sys.error(s"rule '$name' has no labels")
      }

      // pattern is required
      val pattern = try {
        m("pattern").toString()
      } catch {
        case e: Exception => sys.error(s"rule '$name' has no pattern")
      }

      // these fields have default values
      val ruleType = m.getOrElse("type", DefaultType).toString()
      val priority = m.getOrElse("priority", DefaultPriority).toString()
      val keep = if (m contains "keep") m("keep").asInstanceOf[Boolean] else DefaultKeep
      val action = m.getOrElse("action", DefaultAction).toString()

      // make intermediary rule
      new Rule(name, labels, ruleType, priority, keep, action, pattern)
    }
  }

  // compiles a rule into an extractor
  private def mkExtractor(rule: Rule): Extractor = {
    try {
      rule.ruleType match {
        case "token" => mkTokenExtractor(rule)
        case "dependency" => mkDependencyExtractor(rule)
        case _ => sys.error(s"rule '${rule.name}' has an invalid type")
      }
    } catch {
      case e: Exception => sys.error(s"Error parsing rule '${rule.name}': ${e.getMessage}")
    }
  }

  // compiles a token extractor
  private def mkTokenExtractor(rule: Rule): TokenExtractor = {
    val name = rule.name
    val labels = rule.labels
    val priority = Priority(rule.priority)
    val keep = rule.keep
    val action = mirror.reflect(rule.action)
    val pattern = TokenPattern.compile(rule.pattern)
    // FIXME we should use all labels
    new TokenExtractor(name, labels.head, priority, keep, action, pattern)
  }

  // compiles a dependency extractor
  private def mkDependencyExtractor(rule: Rule): DependencyExtractor = {
    val name = rule.name
    val labels = rule.labels
    val priority = Priority(rule.priority)
    val keep = rule.keep
    val action = mirror.reflect(rule.action)
    val pattern = DependencyPattern.compile(rule.pattern)
    // FIXME we should use all labels
    new DependencyExtractor(name, labels.head, priority, keep, action, pattern)
  }
}

object RuleReader {
  val DefaultType = "dependency"
  val DefaultPriority = "1+"
  val DefaultKeep = true
  val DefaultAction = "identity"

  def apply[A <: Actions : ClassTag](actions: A): RuleReader[A] = new RuleReader(actions)
}
