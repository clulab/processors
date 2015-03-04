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
    // all rules need a name
    if (rules exists (!_.contains("name"))) sys.error("unnamed rule")
    // count names occurrences
    val names = rules groupBy (_("name")) mapValues (_.size)
    // names should be unique
    names find (_._2 > 1) match {
      case None => rules map mkExtractor  // return extractors
      case Some((name, count)) => sys.error(s"rule name '$name' is not unique")
    }
  }

  private def readRules(input: String): Seq[Map[String, String]] = {
    val yaml = new Yaml(new Constructor(classOf[Collection[JMap[String, Any]]]))
    val rules = yaml.load(input).asInstanceOf[Collection[JMap[String, Any]]]
    rules.asScala.toSeq.map(_.asScala.toMap.mapValues(_.toString))
  }

  private def mkExtractor(rule: Map[String, String]): Extractor = {
    val name = rule("name")
    try {
      rule.getOrElse("type", DefaultType) match {
        case "token" => mkTokenExtractor(rule)
        case "dependency" => mkDependencyExtractor(rule)
        case _ => sys.error("invalid type")
      }
    } catch {
      case e: Exception => sys.error(s"Error parsing rule '$name': ${e.getMessage}")
    }
  }

  private def mkTokenExtractor(rule: Map[String, String]): TokenExtractor = {
    val name = rule("name")
    val label = rule("label")
    val priority = Priority(rule.getOrElse("priority", DefaultPriority))
    val keep = keepValue(rule.getOrElse("keep", DefaultKeep))
    val action = mirror.reflect(rule.getOrElse("action", DefaultAction))
    val pattern = TokenPattern.compile(rule("pattern"))
    new TokenExtractor(name, label, priority, keep, action, pattern)
  }

  private def mkDependencyExtractor(rule: Map[String, String]): DependencyExtractor = {
    val name = rule("name")
    val label = rule("label")
    val priority = Priority(rule.getOrElse("priority", DefaultPriority))
    val keep = keepValue(rule.getOrElse("keep", DefaultKeep))
    val action = mirror.reflect(rule.getOrElse("action", DefaultAction))
    val pattern = DependencyPattern.compile(rule("pattern"))
    new DependencyExtractor(name, label, priority, keep, action, pattern)
  }

  private def keepValue(s: String): Boolean = s match {
    case "true" => true
    case "false" => false
    case s => sys.error(s"invalid keep value '$s'")
  }
}

object RuleReader {
  val DefaultType = "dependency"
  val DefaultPriority = "1+"
  val DefaultKeep = "true"
  val DefaultAction = "identity"

  def apply[A <: Actions : ClassTag](actions: A): RuleReader[A] = new RuleReader(actions)
}
