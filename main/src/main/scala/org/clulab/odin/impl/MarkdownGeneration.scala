package org.clulab.odin.impl

import org.clulab.odin.impl.MarkdownGeneration._
import org.clulab.odin.impl.RuleReader.{DefaultAction, Rule}

import scala.collection.mutable.ArrayBuffer

case class RuleSchema(
  name: String,
  extractorType: String,
  labels: Seq[String],
  priority: String,
  action: Option[String],
  keep: Boolean,
  additional: Map[String, String],
  arguments: Seq[ArgumentSchema]
) {
  def toMarkdown: String = {
    val preamble = List(
      "--------", // hline
      "",
      s"#### rule: _${name}_",
      "",
      s"attribute | value",
      "-----  |   ---- "
    )

    val table = ArrayBuffer[String](
      s"type |  ${extractorType}",
      s"labels    | ${labelsString(labels)}",
      s"priority  | ${priority}",
      s"keep      | ${booleanString(keep)}"
    )
    if (action.isDefined) table.append(s"action | `Action` | `${action}` ")

    // additional -- these are for cross-sentence rules
    additional.foreach{ case (key, value) => table.append(s"$key  | $value") }

    table.append("")

    // args
    val argumentLines = if (arguments.isEmpty) {
      List(
        "",
        "_No arguments_"
      )
    } else {
      List(
        "",
        "**argument name** | **label(s)** | **quantifier** | **required?**",
        ":---- | :---- | :---- | :----"
      ) ++ arguments.map(_.toMarkdown)
    }

    val lines = preamble ++ table ++ argumentLines ++ Seq("", "&nbsp;", "")
    lines.mkString("\n")
  }
}

case class ExtractionSchema(
   name: String,
   rules: Seq[String],
   labels: Seq[String],
   priorities: Seq[String],
   actions: Seq[String],
   keep: Seq[Boolean],
   argumentsPerRule: Seq[Seq[ArgumentSchema]]
) {
  val argsByName = argumentsPerRule.flatten.groupBy(_.name)
  val aggregatedArgs = aggregateArgs()

  def toMarkdown(minimal: Boolean = false): String = {

    val table = ArrayBuffer[String](
      "----------------------------------",
      "",
      s"###  ${name}",
      "",
      s"|Attribute        |  Value | ",
      s"| :--------       | :---- |",
      s"|label hierarchy  | ${labelsString(labels)} "
    )
    if (!minimal) {
      table.append(s"|rules            | ${listString(rules.map(r => s"""_${r}_"""))} ")
      table.append(s"|priorities       | ${listString(priorities)}")
    }
    table.append(s"|keep             | ${booleanString(keep)} ")

    if (actions.nonEmpty && !minimal) table.append(s"|actions | ${backtickedString(actions)}")

    // arguments
    if (argsByName.nonEmpty) {
      table.appendAll(
        List(
          "",
          "_Arguments_",
          "",
          "|name        | **label(s)**  | **quantifier(s)** | **required?**|",
          "| :--------  | :----         | :----             | :---- "
        )
      )
      val argLines = for {
        (name, (labels, possibleQuantifiers, optionalities)) <- aggregatedArgs
      } yield s"| _${name}_ | ${labels} | ${possibleQuantifiers} | ${optionalities}"
      table.appendAll(argLines)
    }
    else {
      table.appendAll(List("", "_No arguments_"))
    }
    table.append("")
    table.mkString("\n")
  }

  def aggregateArgs(): Map[String, (String, String, String)] = {
    for {
      (name, argSchemas) <- argsByName
    } yield (name, aggregateArgs(name, argSchemas))
  }
  def aggregateArgs(name: String, argSchemas: Seq[ArgumentSchema]): (String, String, String) = {
    val labels = labelsString(argSchemas.map(_.label).distinct)
    val possibleQuantifiers = listString(argSchemas.map(_.quantifier))
    val requirednesses = {
      val requiredValues = argSchemas.map(_.required)
      // Another way an argument can be not required is if it's not extracted in all rules
      // for Mentions of this type.
      // TODO: test this logic!!
      val inAllRules = argumentsPerRule.forall {
        rule => // for this rule, there is an argument with this name
          // and it's required
          rule.exists(arg => arg.name == name && arg.required)
      }
      booleanString(requiredValues ++ Seq(inAllRules))
    }
    (labels, possibleQuantifiers, requirednesses)
  }

}

case class ArgumentSchema(
  name: String,
  label: String,
  quantifier: String,
  required: Boolean
) {
  // each argument is a row in an argument table
  def toMarkdown: String =
    s" ${name} | " +
    s"`${label}` | " +
    s"${quantifier} | " +
    s"${booleanString(required)} "
}



object MarkdownGeneration {

  // -----------------------------------------------------------------------
  //          Methods for autogeneration of grammar documentation
  // -----------------------------------------------------------------------

  /**
    * Convert a single rule to a markdown schema representation
    * @param rule
    * @param extractor
    * @return markdown representation
    */
  def toRuleSchema(rule: Rule, extractor: Extractor): RuleSchema = {
    extractor match {
      case x: CrossSentenceExtractor => crossSentenceRuleSchema(rule, x)
      case x: TokenExtractor => tokenExtractorRuleSchema(rule, x)
      case x: GraphExtractor => graphExtractorRuleSchema(rule, x)
      case _ => ???
    }
  }


  def crossSentenceRuleSchema(r: Rule, x: CrossSentenceExtractor): RuleSchema = {
    RuleSchema(
      name = r.name,
      extractorType = "CrossSentenceExtractor",
      labels = x.labels,
      priority = priorityString(x.priority),
      action = if (r.action != DefaultAction) Some(r.action) else None,
      keep = x.keep,
      additional = Map(
        "leftWindow" -> x.leftWindow.toString,
        "rightWindow" -> x.rightWindow.toString,
        "anchorRole" -> x.anchorRole,
        "neighborRole" -> x.neighborRole
      ),
      Seq.empty
    )
  }

  def tokenExtractorRuleSchema(r: Rule, x: TokenExtractor): RuleSchema = {
    RuleSchema(
      name = r.name,
      extractorType = "TokenExtractor",
      labels = x.labels,
      priority = priorityString(x.priority),
      action = if (r.action != DefaultAction) Some(r.action) else None,
      keep = x.keep,
      additional = Map.empty,
      arguments = Seq.empty
    )
  }

  /** With GraphExtractors, the patterns support arguments and config variables too,
    * so here we display those as well */
  def graphExtractorRuleSchema(r: Rule, x: GraphExtractor): RuleSchema = {
    RuleSchema(
      name = r.name,
      extractorType = "GraphExtractor",
      labels = x.labels,
      priority = priorityString(x.priority),
      action = if (r.action != DefaultAction) Some(r.action) else None,
      keep = x.keep,
      additional = Map.empty,
      arguments = toArgSchema(x.pattern.arguments)
    )
  }

  // Arguments

  /**
    * Create the markdown strings for all the arguments in a rule
    * @param args ArgumentPatterns for the Rule
    * @return markdown table strings
    */
  def toArgSchema(args: Seq[ArgumentPattern]): Seq[ArgumentSchema] = {
    args.map(toArgSchema)
  }

  /**
    * Create the markdown table line for a given argument, including the name, the label,
    * whether or not the argument was quantified, and if it's optional.
    * @param a
    * @return markdown table string
    */
  def toArgSchema(a: ArgumentPattern): ArgumentSchema = {
    ArgumentSchema(
      name = a.name,
      label = a.label,
      quantifier = quantifierString(a.quantifier),
      required = a.required
    )
  }

  /**
    * write the map entries within a dropdown summary
    * @param map The config variables map used for string replacement.  Typically this holds the values
    *            for things like triggers, vars, etc.
    * @return markdown strings
    */
  def configVariableSummary(map: Map[String, String]): List[String] = {
    val preamble = List (
      "<details>",
      "<summary>Config variables</summary>",
      "",
      "**Attribute** | **Type** | **Value**",
      "----|----|----"
    )
    val keys = map.keys.toList.sortWith(_ < _) // alphasort
    val body = keys.map( key =>  "%s | String | %s".format(key, map(key)) )

    val postamble = List (
      "",
      "",
      "</details>",
      "",
      ""
    )

    List(preamble, body, postamble).flatten
  }

  // ------------------------------------------
  //      Methods for aggregated view
  // ------------------------------------------

  /**
    *
    * @param label the internal taxonomy label that all mentions produced
    *              by these rules conform to
    * @param pairs the (Rule, Extractor) pairs for all rules that
    *              extract mentions with label == `label`
    * @return sequence of ExtractionSchema
    */
  def toExtractionSchema(label: String, pairs: Seq[(Rule, Extractor)]): ExtractionSchema = {
    val (rules, extractors) = pairs.unzip
    assert(pairs.nonEmpty) // Should not happen

    val arguments = extractors
      .collect{ case gp: GraphExtractor => gp.pattern.arguments}
      .map(toArgSchema)

    ExtractionSchema(
      name = label,
      rules = rules.map(_.name),
      // All should have the same labels because they come from the taxonomy
      // assert(labels.length == extractors.flatMap(_.labels).distinct.length)
      labels = extractors.head.labels,
      priorities = extractors.map(e => priorityString(e.priority)),
      actions = rules.map(_.action).filterNot(_ == DefaultAction),
      keep = extractors.map(_.keep),
      argumentsPerRule = arguments
    )
  }


  def listString(ss: Seq[String]): String = {
    val distinct = ss.distinct.sorted
    distinct.length match {
      case 0 => ""
      case 1 => distinct.head
      case _ => s"[${distinct.mkString(", ")}]"
    }
  }

  def quantifierString(q: ArgumentQuantifier): String = {
    q match {
      case NullQuantifier => "_none_"
      case e:ExactQuantifier => s"`${e.reps}`"
      case r: RangedQuantifier =>
        (r.minRepeat, r.maxRepeat) match {
          case (None, Some(1)) => s"`?`"
          case (None, Some(q)) => s"`0-$q`"
          case (Some(q1), Some(q2)) => s"`$q1-$q2`"
          case (Some(q), None) => s"`$q+`"
          case _ => ???
        }
      case _ => ???
    }
  }

  def priorityString(p: Priority): String = {
    p match {
      case e: ExactPriority => s"`${e.value.toString}`"
      case i: IntervalPriority => s"`(${i.start}-${i.end})`"
      case l: LowerBoundPriority => s"`${l.start}+`"
      case s: SparsePriority => s"`{${s.values.mkString(", ")}}`"
      case _ => ???
    }
  }

  def booleanString(bb: Seq[Boolean]): String =
    listString(bb.map(booleanString))
  def booleanString(b: Boolean): String = s"`${b}`"

  def labelsString(ll: Seq[String]): String = {
    val backticked = ll.map(l => s"`${l}`")
    s"[${backticked.mkString(", ")}]"
  }

  def backtickedString(aa: Seq[Any]): String = {
    val backticked = aa.map(a => s"`${a}`")
    listString(backticked)
  }

}
