package edu.arizona.sista.odin.impl

import java.io.File
import java.util.{ Collection, Map => JMap }
import scala.reflect.ClassTag
import scala.beans.BeanProperty
import scala.collection.JavaConverters._
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.{ Constructor, ConstructorException }
import edu.arizona.sista.odin._

class RuleReader[A <: Actions : ClassTag](val actions: A) {

  import RuleReader._

  // invokes actions through reflection
  private val mirror = new ActionMirror(actions)

  def read(input: String): Seq[Extractor] =
    try {
      readMasterFile(input)
    } catch {
      case e: ConstructorException => readSimpleFile(input)
    }

  def readSimpleFile(input: String): Seq[Extractor] = {
    val yaml = new Yaml(new Constructor(classOf[Collection[JMap[String, Any]]]))
    val jRules = yaml.load(input).asInstanceOf[Collection[JMap[String, Any]]]
    val rules = readRules(jRules, None, Map.empty)
    mkExtractors(rules)
  }

  def readMasterFile(input: String): Seq[Extractor] = {
    val yaml = new Yaml(new Constructor(classOf[JMap[String, Any]]))
    val master = yaml.load(input).asInstanceOf[JMap[String, Any]].asScala
    val taxonomy = master.get("taxonomy").map(t => Taxonomy(t.asInstanceOf[Collection[Any]]))
    val vars = master.get("vars").map(_.asInstanceOf[JMap[String, String]].asScala.toMap).getOrElse(Map.empty)
    val jRules = master("rules").asInstanceOf[Collection[JMap[String, Any]]]
    val rules = readRules(jRules, taxonomy, vars)
    mkExtractors(rules)
  }

  def mkExtractors(rules: Seq[Rule]): Seq[Extractor] = {
    // count names occurrences
    val names = rules groupBy (_.name) transform ((k, v) => v.size)
    // names should be unique
    names find (_._2 > 1) match {
      case None => rules map mkExtractor // return extractors
      case Some((name, count)) =>
        throw OdinNamedCompileException(s"rule name '$name' is not unique", name)
    }
  }

  def mkRule(
      data: Map[String, Any],
      expand: String => Seq[String],
      template: Any => String
  ): Rule = {

    // name is required
    val name = try {
      template(data("name"))
    } catch {
      case e: Exception => throw OdinCompileException("unnamed rule")
    }

    // one or more labels are required
    val labels: Seq[String] = try {
      data("label") match {
        case label: String => expand(template(label))
        case jLabels: Collection[_] =>
          jLabels.asScala.flatMap(l => expand(template(l))).toSeq.distinct
      }
    } catch {
      case e: Exception =>
        throw OdinNamedCompileException(s"rule '$name' has no labels", name)
    }

    // pattern is required
    val pattern = try {
      template(data("pattern"))
    } catch {
      case e: Exception =>
        throw OdinNamedCompileException(s"rule '$name' has no pattern", name)
    }

    // these fields have default values
    val ruleType = template(data.getOrElse("type", DefaultType))
    val priority = template(data.getOrElse("priority", DefaultPriority))
    val action = template(data.getOrElse("action", DefaultAction))
    val keep = strToBool(template(data.getOrElse("keep", DefaultKeep)))
    // unit is relevant to TokenPattern only
    val unit = template(data.getOrElse("unit", DefaultUnit))

    // make intermediary rule
    new Rule(name, labels, ruleType, unit, priority, keep, action, pattern)

  }

  private def readRules(
      rules: Collection[JMap[String, Any]],
      taxonomy: Option[Taxonomy],
      vars: Map[String, String]
  ): Seq[Rule] = {

    // return Rule objects
    rules.asScala.toSeq.flatMap { r =>
      val m = r.asScala.toMap
      if (m contains "import") {
        // import rules from a file and return them
        importRules(m, taxonomy, vars)
      } else {
        // gets a label and returns it and all its hypernyms
        val expand: String => Seq[String] = label => taxonomy match {
          case Some(t) => t.hypernymsFor(label)
          case None => Seq(label)
        }
        // interpolates a template variable
        val template: Any => String =
          s => """\$\{(.*)\}""".r.replaceAllIn(s.toString(), m => vars(m.group(1).trim))
        // return the rule (in a Seq because this is a flatMap)
        Seq(mkRule(m, expand, template))
      }
    }

  }

  private def importRules(
      data: Map[String, Any],
      taxonomy: Option[Taxonomy],
      vars: Map[String, String]
  ): Seq[Rule] = {
    val file = new File(data("import").toString)
    val input = io.Source.fromFile(file).mkString
    val (jRules: Collection[JMap[String, Any]], jVars: Map[String, String]) = try {
      val yaml = new Yaml(new Constructor(classOf[JMap[String, Any]]))
      val data = yaml.load(input).asInstanceOf[JMap[String, Any]].asScala.toMap
      val jRules = data("rules").asInstanceOf[Collection[JMap[String, Any]]]
      val jVars = data.get("vars").map(_.asInstanceOf[JMap[String, String]].asScala.toMap).getOrElse(Map.empty)
      (jRules, jVars)
    } catch {
      case e: ConstructorException =>
        val yaml = new Yaml(new Constructor(classOf[Collection[JMap[String, Any]]]))
        val jRules = yaml.load(input).asInstanceOf[Collection[JMap[String, Any]]]
        (jRules, Map.empty)
    }
    val localVars = data.get("vars").map(_.asInstanceOf[JMap[String, String]].asScala).getOrElse(Map.empty)
    readRules(jRules, taxonomy, jVars ++ vars ++ localVars)
  }

  // compiles a rule into an extractor
  private def mkExtractor(rule: Rule): Extractor = {
    try {
      rule.ruleType match {
        case "token" => mkTokenExtractor(rule)
        case "dependency" => mkDependencyExtractor(rule)
        case _ => 
          val msg = s"rule '${rule.name}' has unsupported type '${rule.ruleType}'"
          throw OdinNamedCompileException(msg, rule.name)
      }
    } catch {
      case e: Exception =>
        val msg = s"Error parsing rule '${rule.name}': ${e.getMessage}"
        throw OdinNamedCompileException(msg, rule.name)
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
  val DefaultKeep = "true"
  val DefaultAction = "default"
  val DefaultUnit = "word"

  def strToBool(s: String): Boolean = s match {
    case "y" | "Y" | "yes" | "Yes" | "YES" | "true" | "True" | "TRUE" | "on" | "On" | "ON" => true
    case "n" | "N" | "no" | "No" | "NO" | "false" | "False" | "FALSE" | "off" | "Off" | "OFF" => false
    case b => sys.error(s"invalid boolean literal '$b'")
  }

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
