package org.clulab.odin.impl

import java.net.URL
import java.util.{ Collection, Map => JMap }
import java.nio.charset.Charset

import org.apache.commons.text.StrSubstitutor

import scala.collection.JavaConverters._
import scala.io.{ Codec, Source }

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.{ Constructor, ConstructorException }
import org.clulab.odin._


class RuleReader(val actions: Actions, val charset: Charset) {

  import RuleReader._

  // codec to be used by scala.io.Source
  implicit val codec: Codec = new Codec(charset)

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
    val config = OdinConfig(resources = resources)
    val rules = readRules(jRules, config)
    mkExtractors(rules)
  }

  def readMasterFile(input: String): Vector[Extractor] = {
    val yaml = new Yaml(new Constructor(classOf[JMap[String, Any]]))
    val master = yaml.load(input).asInstanceOf[JMap[String, Any]].asScala.toMap
    val taxonomy = master.get("taxonomy").map(readTaxonomy)
    val vars = getVars(master)
    val resources = readResources(master)
    val jRules = master("rules").asInstanceOf[Collection[JMap[String, Any]]]
    val graph = getGraph(master)
    val config = OdinConfig(taxonomy = taxonomy, resources = resources, variables = vars, graph = graph)
    val rules = readRules(jRules, config)
    mkExtractors(rules)
  }

  def getGraph(
    data: Map[String, Any],
    default: String = OdinConfig.DEFAULT_GRAPH
  ): String = data.get("graph") match {
    case None => default
    case Some(g) =>
      val graph = g.asInstanceOf[String]
      // graph must be of the registered types
      if (! OdinConfig.VALID_GRAPHS.contains(graph))
        throw OdinException(s"'$graph' is not a valid graph type. Valid options: ${
          OdinConfig.VALID_GRAPHS.map(s => s"'$s'").mkString(", ")
        }")
      graph
  }

  // StrSubstitutor doesn't support whitespace in var (ex. ${ varName } )
  private def cleanVar(s: String): String = {
    val clean = s.
      replaceAll("\\$\\{\\s+", "\\$\\{").
      replaceAll("\\s+\\}", "\\}")
    clean
  }

  def getVars(data: Map[String, Any]): Map[String, String] = {
    data
      .get("vars")
      .map(_.asInstanceOf[JMap[String, Any]].asScala.mapValues(s => cleanVar(s.toString)).toMap)
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
      config: OdinConfig
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
    // the graph to match against
    val graph: String = getGraph(data, config.graph)
    val updatedConfig = config.copy(graph = graph)
    // make intermediary rule
    ruleType match {
      case DefaultType =>
        new Rule(name, labels, ruleType, unit, priority, keep, action, pattern, updatedConfig)
      // ignore specification of 'graph' when using "type: dependency"
      case "dependency" =>
        new Rule(name, labels, ruleType, unit, priority, keep, action, pattern, updatedConfig.copy(graph = OdinConfig.DEFAULT_GRAPH))
      case "token" =>
        new Rule(name, labels, ruleType, unit, priority, keep, action, pattern, updatedConfig)
      // cross-sentence cases
      case "cross-sentence" =>
        (data.getOrElse("left-window", None), data.getOrElse("right-window", None)) match {
          case (leftWindow: Int, rightWindow: Int) =>
            new CrossSentenceRule(name, labels, ruleType, leftWindow, rightWindow, unit, priority, keep, action, pattern, updatedConfig)
          case (leftWindow: Int, None) =>
            new CrossSentenceRule(name, labels, ruleType, leftWindow, DefaultWindow, unit, priority, keep, action, pattern, updatedConfig)
          case (None, rightWindow: Int) =>
            new CrossSentenceRule(name, labels, ruleType, DefaultWindow, rightWindow, unit, priority, keep, action, pattern, updatedConfig)
          case _ =>
            throw OdinCompileException(s""""cross-sentence" rule '$name' requires a "left-window" and/or "right-window"""")
        }
      // unrecognized rule type
      case other =>
        throw OdinCompileException(s"""type '$other' not recognized for rule '$name'""")
    }
  }

  private def readRules(
      rules: Collection[JMap[String, Any]],
      config: OdinConfig
  ): Seq[Rule] = {

    // return Rule objects
    rules.asScala.toSeq.flatMap { r =>
      val m = r.asScala.toMap
      if (m contains "import") {
        // import rules from a file and return them
        importRules(m, config)
      } else {
        // gets a label and returns it and all its hypernyms
        val expand: String => Seq[String] = label => config.taxonomy match {
          case Some(t) => t.hypernymsFor(label)
          case None => Seq(label)
        }
        // interpolates a template variable with ${variableName} notation
        // note that $variableName is not supported and $ can't be escaped
        val template: Any => String = a => replaceVars(a.toString, config.variables)
        // return the rule (in a Seq because this is a flatMap)
        Seq(mkRule(m, expand, template, config))
      }
    }

  }

  // reads a taxonomy from data, where data may be either a forest or a file path
  private def readTaxonomy(data: Any): Taxonomy = data match {
    case t: Collection[_] => Taxonomy(t.asInstanceOf[Collection[Any]])
    case path: String =>
      val url = mkURL(path)
      val source = Source.fromURL(url)
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
      config: OdinConfig
  ): Seq[Rule] = {
    // apply variable substitutions to import
    val path = {
      val p = data("import").toString
      val res = replaceVars(p, config.variables)
      res
    }
    val url = mkURL(path)
    val source = Source.fromURL(url)
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
    val updatedVars = localVars ++ config.variables ++ importVars
    val newConf = config.copy(variables = updatedVars)
    readRules(jRules, newConf)
  }

  private def replaceVars(s: String, vars: Map[String, String]): String = {
    val valuesMap = vars.asJava
    // NOTE: StrSubstitutor is NOT threadsafe
    val sub = new StrSubstitutor(valuesMap)
    // allow for recursive substitution
    sub.setEnableSubstitutionInVariables(true)
    val clean = cleanVar(s)
    sub.replace(clean)
  }

  // compiles a rule into an extractor
  private def mkExtractor(rule: Rule): Extractor = {
    try {
      rule.ruleType match {
        case "token" => mkTokenExtractor(rule)
        case DefaultType => mkGraphExtractor(rule)
        // FIXME: should this be deprecated?
        case "dependency" => mkGraphExtractor(rule)
        case "cross-sentence" => mkCrossSentenceExtractor(rule)
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
    val compiler = new TokenPatternParsers(rule.unit, rule.config)
    val pattern = compiler.compileTokenPattern(rule.pattern)
    new TokenExtractor(name, labels, priority, keep, action, pattern)
  }

  private def mkCrossSentenceExtractor(rule: Rule): CrossSentenceExtractor = {

    val lw: Int = rule match {
      case csr: CrossSentenceRule => csr.leftWindow
      case other => DefaultWindow
    }

    val rw: Int = rule match {
      case csr: CrossSentenceRule => csr.rightWindow
      case other => DefaultWindow
    }

    // convert cross-sentence pattern...
    // pattern: |
    //   argName1:ArgType = tokenpattern
    //   argName2: ArgType = tokenpattern
    // ...to Seq[(role, Rule)]
    val rolesWithRules: Seq[(String, Rule)] = for {
      (argPattern, i) <- rule.pattern.split("\n").zipWithIndex
      // ignore empty lines
      if argPattern.trim.nonEmpty
      // ignore comments
      if ! argPattern.trim.startsWith("#")
    } yield {
      // split arg pattern into 'argName1:ArgType', 'tokenpattern'
      // apply split only once
      val contents: Seq[String] = argPattern.split("\\s*=\\s*", 2)
      if (contents.size != 2) throw OdinException(s"'$argPattern' for rule '${rule.name}' must have the form 'argName:ArgType = tokenpattern'")
      // split 'argName1:ArgType' into 'argName1', 'ArgType'
      // apply split only once
      val argNameContents = contents.head.split("\\s*:\\s*", 2)
      if (argNameContents.size != 2) throw OdinException(s"'${contents.head}' for rule '${rule.name}' must have the form 'argName:ArgType'")
      val role = argNameContents.head.trim
      val label = argNameContents.last.trim
      // pattern for argument
      val pattern = contents.last.trim
      // make rule name
      val ruleName = s"${rule.name}_arg:$role"
      // labels from label
      val labels = rule.taxonomy match {
        case Some(t) => t.hypernymsFor(label)
        case None => Seq(label)
      }
      //println(s"labels for '$ruleName' with pattern '$pattern': '$labels'")
      // Do not apply cross-sentence rule's action to anchor and neighbor
      // This does not need to be stored
      (role, new Rule(ruleName, labels, "token", rule.unit, rule.priority, false, DefaultAction, pattern, rule.config))
    }

    if (rolesWithRules.size != 2) throw OdinException(s"Pattern for '${rule.name}' must contain exactly two args")

    new CrossSentenceExtractor(
      name = rule.name,
      labels = rule.labels,
      priority = Priority(rule.priority),
      keep = rule.keep,
      action = mirror.reflect(rule.action),
      // the maximum number of sentences to look behind for pattern2
      leftWindow = lw,
      // the maximum number of sentences to look ahead for pattern2
      rightWindow = rw,
      anchorPattern = mkTokenExtractor(rolesWithRules.head._2),
      neighborPattern = mkTokenExtractor(rolesWithRules.last._2),
      anchorRole = rolesWithRules.head._1,
      neighborRole = rolesWithRules.last._1
    )
  }

  // compiles a dependency extractor
  private def mkGraphExtractor(rule: Rule): GraphExtractor = {
    val name = rule.name
    val labels = rule.labels
    val priority = Priority(rule.priority)
    val keep = rule.keep
    val action = mirror.reflect(rule.action)
    val compiler = new GraphPatternCompiler(rule.unit, rule.config)
    val pattern = compiler.compileGraphPattern(rule.pattern)
    new GraphExtractor(name, labels, priority, keep, action, pattern, rule.config)
  }
}

object RuleReader {
  val DefaultType = "graph"
  val DefaultPriority = "1+"
  val DefaultWindow = 0 // 0 means don't use a window
  val DefaultKeep = "true"
  val DefaultAction = "default"
  val DefaultUnit = "word"

  // interprets yaml boolean literals
  def strToBool(s: String): Boolean = s match {
    case "y" | "Y" | "yes" | "Yes" | "YES" | "true" | "True" | "TRUE" | "on" | "On" | "ON" => true
    case "n" | "N" | "no" | "No" | "NO" | "false" | "False" | "FALSE" | "off" | "Off" | "OFF" => false
    case b => sys.error(s"invalid boolean literal '$b'")
  }

  val CLASSPATH_PROTOCOL = "classpath:"

  /**
    * Makes a url from an odin import that considers the custom classpath protocol
    * @param path: String representing the URL
    * @return URL
    */
  def mkURL(path: String): URL = {
    // check if path is relative to resources
    val resource = getClass.getClassLoader.getResource(path)
    path match {
      case hasResource if resource != null => resource
      case cp if cp startsWith CLASSPATH_PROTOCOL =>
        val path = cp.drop(CLASSPATH_PROTOCOL.length)
        getClass.getResource(path)
      case other => new URL(other)
    }
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
      val config: OdinConfig
  ) {

    val taxonomy: Option[Taxonomy] = config.taxonomy
    val resources = config.resources

    def copy(
      name: String = this.name,
      labels: Seq[String] = this.labels,
      ruleType: String = this.ruleType,
      unit: String = this.unit,
      priority: String = this.priority,
      keep: Boolean = this.keep,
      action: String = this.action,
      pattern: String = this.pattern,
      config: OdinConfig = this.config
    ): Rule = new Rule(name, labels, ruleType, unit, priority, keep, action, pattern, config)
  }

  /**
    * Intermediate representation for a rule spanning multiple sentences
    * Produces a RelationMention with exactly two args: anchor and neighbor
    */
  class CrossSentenceRule(
    name: String,
    labels: Seq[String],
    ruleType: String,
    // the maximum number of sentences to look behind for pattern2
    val leftWindow: Int,
    // the maximum number of sentences to look ahead for pattern2
    val rightWindow: Int,
    unit: String,
    priority: String,
    keep: Boolean,
    action: String,
    pattern: String,
    config: OdinConfig
  ) extends Rule(name, labels, ruleType, unit, priority, keep, action, pattern, config)
}
