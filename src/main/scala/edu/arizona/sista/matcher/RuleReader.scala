package edu.arizona.sista.matcher

import java.util.{ Collection, Map => JMap }
import scala.reflect.BeanProperty
import scala.collection.JavaConverters._
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

class RuleReader[T <: Actions](val actions: T) {
  // invokes actions through reflection
  private val mirror = new ActionMirror(actions)

  def read(input: String): Seq[Extractor] =
    readRules(input) map mkExtractor

  private def readRules(input: String): Seq[Map[String,String]] = {
    val yaml = new Yaml(new Constructor(classOf[Collection[JMap[String,Any]]]))
    val rules = yaml.load(input).asInstanceOf[Collection[JMap[String,Any]]]
    rules.asScala.toSeq.map(_.asScala.toMap.mapValues(_.toString))
  }

  private def mkExtractor(rule: Map[String,String]): Extractor = {
    require(rule.contains("name"), "unnamed rule")
    val name = rule("name")
    try {
      rule.getOrElse("type", "dependency") match {
        case "token" => mkTokenExtractor(rule)
        case "dependency" => mkDependencyExtractor(rule)
        case _ => sys.error("invalid type")
      }
    } catch {
      case e: Exception => sys.error(s"""Error parsing rule "$name": ${e.getMessage}""")
    }
  }

  private def mkTokenExtractor(rule: Map[String,String]): TokenExtractor = {
    val name = rule("name")
    val label = rule("label")
    val priority = Priority(rule.getOrElse("priority", "1+"))
    val action = mirror.reflect(rule("action"))
    val pattern = TokenPattern.compile(rule("pattern"))
    new TokenExtractor(name, label, priority, action, pattern)
  }

  private def mkDependencyExtractor(rule: Map[String,String]): DependencyExtractor = {
    val name = rule("name")
    val label = rule("label")
    val priority = Priority(rule.getOrElse("priority", "1+"))
    val action = mirror.reflect(rule("action"))
    val pattern = DependencyPattern.compile(rule("pattern"))
    new DependencyExtractor(name, label, priority, action, pattern)
  }
}
