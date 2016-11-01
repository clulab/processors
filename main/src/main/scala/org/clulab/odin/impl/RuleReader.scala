package org.clulab.odin.impl

import java.util.{ Collection, Map => JMap }

import scala.collection.JavaConverters._
import scala.util.matching.Regex
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.{ Constructor, ConstructorException }
import org.clulab.odin._

class RuleReader(val actions: Actions) {

  import RuleReader._

  // invokes actions through reflection
  private val mirror = new ActionMirror(actions)

  def read(input: String): Vector[Extractor] =
    try {
      readMasterFile(input)
    } catch {
      case e: ConstructorException => readSimpleFile(input)
    }

  def readSimpleFile(input: String): Vector[Extractor] = {
    val yaml = new Yaml(new Constructor(classOf[Collection[JMap[String, Any]]]))
    val jRules = yaml.load(input).asInstanceOf[Collection[JMap[String, Any]]]
    // no resources are specified
    val resources = OdinResourceManager(Map.empty)
    val rules = readRules(jRules, None, Map.empty, resources)
    mkExtractors(rules)
  }

  def readMasterFile(input: String): Vector[Extractor] = {
    val yaml = new Yaml(new Constructor(classOf[JMap[String, Any]]))
    val master = yaml.load(input).asInstanceOf[JMap[String, Any]].asScala.toMap
    val taxonomy = master.get("taxonomy").map(readTaxonomy)
    val vars = getVars(master)
    val resources = readResources(master)
    val jRules = master("rules").asInstanceOf[Collection[JMap[String, Any]]]
    val rules = readRules(jRules, taxonomy, vars, resources)
    mkExtractors(rules)
  }

  def getVars(data: Map[String, Any]): Map[String, String] = {
    data
      .get("vars")
      .map(_.asInstanceOf[JMap[String, Any]].asScala.mapValues(_.toString).toMap)
      .getOrElse(Map.empty)
  }

  def mkExtractors(rules: Seq[Rule]): Vector[Extractor] = {
    // count names occurrences
    val names = rules groupBy (_.name) transform ((k, v) => v.size)
    // names should be unique
    names find (_._2 > 1) match {
      case None => (rules map mkExtractor).toVector // return extractors
      case Some((name, count)) =>
        throw new OdinNamedCompileException(s"rule name '$name' is not unique", name)
    }
  }

  def mkRule(
      data: Map[String, Any],
      expand: String => Seq[String],
      template: Any => String,
      resources: OdinResourceManager
  ): Rule = {

    // name is required
    val name = try {
      template(data("name"))
    } catch {
      case e: NoSuchElementException => throw new OdinCompileException("unnamed rule")
    }

    // replace variables or throw an OdinNamedCompileException
    def tmpl(raw: Any): String = {
      try {
        template(raw)
      } catch {
        case e: NoSuchElementException =>
          val varName = e.getMessage.drop(15)
          throw new OdinNamedCompileException(s"var '$varName' not found in rule '$name'", name)
      }
    }

    // one or more labels are required
    val labels: Seq[String] = try {
      data("label") match {
        case label: String => expand(tmpl(label))
        case jLabels: Collection[_] =>
          jLabels.asScala.flatMap(l => expand(tmpl(l))).toSeq.distinct
      }
    } catch {
      case e: NoSuchElementException =>
        throw new OdinNamedCompileException(s"rule '$name' has no labels", name)
    }

    // pattern is required
    val pattern = try {
      tmpl(data("pattern"))
    } catch {
      case e: NoSuchElementException =>
        throw new OdinNamedCompileException(s"rule '$name' has no pattern", name)
    }

    // these fields have default values
    val ruleType = tmpl(data.getOrElse("type", DefaultType))
    val priority = tmpl(data.getOrElse("priority", DefaultPriority))
    val action = tmpl(data.getOrElse("action", DefaultAction))
    val keep = strToBool(tmpl(data.getOrElse("keep", DefaultKeep)))
    // unit is relevant to TokenPattern only
    val unit = tmpl(data.getOrElse("unit", DefaultUnit))

    // make intermediary rule
    new Rule(name, labels, ruleType, unit, priority, keep, action, pattern, resources)

  }

  private def readRules(
      rules: Collection[JMap[String, Any]],
      taxonomy: Option[Taxonomy],
      vars: Map[String, String],
      resources: OdinResourceManager
  ): Seq[Rule] = {

    // return Rule objects
    rules.asScala.toSeq.flatMap { r =>
      val m = r.asScala.toMap
      if (m contains "import") {
        // import rules from a file and return them
        importRules(m, taxonomy, vars, resources)
      } else {
        // gets a label and returns it and all its hypernyms
        val expand: String => Seq[String] = label => taxonomy match {
          case Some(t) => t.hypernymsFor(label)
          case None => Seq(label)
        }
        // interpolates a template variable with ${variableName} notation
        // note that $variableName is not supported and $ can't be escaped
        val template: Any => String = { s =>
          """\$\{\s*(\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*)\s*\}""".r
            .replaceAllIn(s.toString(), m => Regex.quoteReplacement(vars(m.group(1))))
        }
        // return the rule (in a Seq because this is a flatMap)
        Seq(mkRule(m, expand, template, resources))
      }
    }

  }

  // reads a taxonomy from data, where data may be either a forest or a file path
  private def readTaxonomy(data: Any): Taxonomy = data match {
    case t: Collection[_] => Taxonomy(t.asInstanceOf[Collection[Any]])
    case path: String =>
      val url = getClass.getClassLoader.getResource(path)
      val source = if (url == null) io.Source.fromFile(path) else io.Source.fromURL(url)
      val input = source.mkString
      source.close()
      val yaml = new Yaml(new Constructor(classOf[Collection[Any]]))
      val data = yaml.load(input).asInstanceOf[Collection[Any]]
      Taxonomy(data)
  }

  // reads resources from data, and constructs an OdinResourceManager instance
  private def readResources(data: Map[String, Any]): OdinResourceManager = {
    //println(s"resources: ${data.get("resources")}")
    val resourcesMap: Map[String, String] = data.get("resources") match {
      case Some(m: JMap[_, _]) => m.asScala.map(pair => (pair._1.toString, pair._2.toString)).toMap
      case _ => Map.empty
    }
    OdinResourceManager(resourcesMap)
  }


  private def importRules(
      data: Map[String, Any],
      taxonomy: Option[Taxonomy],
      importerVars: Map[String, String],
      resources: OdinResourceManager
  ): Seq[Rule] = {
    val path = data("import").toString
    val url = getClass.getClassLoader.getResource(path)
    // try to read a resource or else a file
    val source = if (url == null) io.Source.fromFile(path) else io.Source.fromURL(url)
    val input = source.mkString // slurp
    source.close()
    // read rules and vars from file
    val (jRules: Collection[JMap[String, Any]], localVars: Map[String, String]) = try {
      // try to read file with rules and optional vars by trying to read a JMap
      val yaml = new Yaml(new Constructor(classOf[JMap[String, Any]]))
      val data = yaml.load(input).asInstanceOf[JMap[String, Any]].asScala.toMap
      // read list of rules
      val jRules = data("rules").asInstanceOf[Collection[JMap[String, Any]]]
      // read optional vars
      val localVars = getVars(data)
      (jRules, localVars)
    } catch {
      case e: ConstructorException =>
        // try to read file with a list of rules by trying to read a Collection of JMaps
        val yaml = new Yaml(new Constructor(classOf[Collection[JMap[String, Any]]]))
        val jRules = yaml.load(input).asInstanceOf[Collection[JMap[String, Any]]]
        (jRules, Map.empty)
    }
    // variables specified by the call to `import`
    val importVars = getVars(data)
    // variable scope:
    // - an imported file may define its own variables (`localVars`)
    // - the importer file can define variables (`importerVars`) that override `localVars`
    // - a call to `import` can include variables (`importVars`) that override `importerVars`
    readRules(jRules, taxonomy, mergeVariables(localVars, importerVars, importVars), resources)
  }

  private def mergeVariables(vs1: Map[String, String], vs2: Map[String, String], vs3: Map[String, String]): Map[String, String] = {
    def template(s: String, vars: Map[String, String]): String = {
      """\$\{\s*(\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*)\s*\}""".r
        .replaceAllIn(s.toString(), m => Regex.quoteReplacement(vars(m.group(1))))
    }
    val vars1 = vs1 ++ vs2.mapValues(template(_, vs2))
    vars1 ++ vs3.mapValues(template(_, vars1))
  }

  // compiles a rule into an extractor
  private def mkExtractor(rule: Rule): Extractor = {
    try {
      rule.ruleType match {
        case "token" => mkTokenExtractor(rule)
        case "dependency" => mkDependencyExtractor(rule)
        case _ =>
          val msg = s"rule '${rule.name}' has unsupported type '${rule.ruleType}'"
          throw new OdinNamedCompileException(msg, rule.name)
      }
    } catch {
      case e: Exception =>
        val msg = s"Error parsing rule '${rule.name}': ${e.getMessage}"
        throw new OdinNamedCompileException(msg, rule.name)
    }
  }

  // compiles a token extractor
  private def mkTokenExtractor(rule: Rule): TokenExtractor = {
    val name = rule.name
    val labels = rule.labels
    val priority = Priority(rule.priority)
    val keep = rule.keep
    val action = mirror.reflect(rule.action)
    val compiler = new TokenPatternParsers(rule.unit, rule.resources)
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
    val compiler = new DependencyPatternCompiler(rule.unit, rule.resources)
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

  // interprets yaml boolean literals
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
      val pattern: String,
      val resources: OdinResourceManager
  )

}
