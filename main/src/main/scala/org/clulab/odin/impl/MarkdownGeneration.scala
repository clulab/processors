package org.clulab.odin.impl

import org.clulab.odin.impl.RuleReader.{DefaultAction, Rule}

import scala.collection.mutable.ArrayBuffer

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
  def toMarkdown(rule: Rule, extractor: Extractor): String = {
    val preamble = List(
      "--------", // hline
      "",
      s"#### rule: _${extractor.name}_",
      "",
      s"attribute | value",
      "-----  |   ---- "
    )

    val lines = extractor match {
      case x: CrossSentenceExtractor => crossSentenceExtractorAsMarkdown(rule, x)
      case x: TokenExtractor => tokenExtractorAsMarkdown(rule, x)
      case x: GraphExtractor => graphExtractorAsMarkdown(rule, x)
      case _ => List()
    }
    val list = preamble ++ lines ++ Seq("", "&nbsp;", "")
    list.mkString("\n")
  }


  def crossSentenceExtractorAsMarkdown(r: Rule, x: CrossSentenceExtractor): List[String] = {
    val table = ArrayBuffer[String](
      "type |  `CrossSentenceExtractor`",
      s"labels    | ${labelsString(x.labels)}",
      s"priority  | ${priorityString(x.priority)}",
      s"keep      | ${booleanString(x.keep)}"
    )
    if (r.action != DefaultAction) table.append(s"action | `Action` | `${r.action}` ")
    table.appendAll(
      List(
        s"leftWindow | ${x.leftWindow}",
        s"rightWindow | ${x.rightWindow}",
        s"anchorRole | ${x.anchorRole}",
        s"neighborRole | ${x.neighborPattern}"
      )
    )
    table.toList
  }

  def tokenExtractorAsMarkdown(r: Rule, x: TokenExtractor): List[String] = {
    val table = ArrayBuffer[String](
      "type | `TokenExtractor`",
      s"labels    | ${labelsString(x.labels)}",
      s"priority  | ${priorityString(x.priority)}",
      s"keep      | ${booleanString(x.keep)}"
    )
    if (r.action != DefaultAction) table.append(s"action | `${r.action}` ")
    table.append("")
    table.toList
  }

  /** With GraphExtractors, the patterns support arguments and config variables too,
    * so here we display those as well */
  def graphExtractorAsMarkdown(r: Rule, x: GraphExtractor): List[String] = {
    val argumentLines = argsToMarkdown(x.pattern.arguments)
    val table = ArrayBuffer[String](
      s"type   | `GraphExtractor` (${x.config.graph})",
      s"labels        |  ${labelsString(x.labels)}",
      s"priority      |  ${priorityString(x.priority)}",
      s"keep          |  ${booleanString(x.keep)}"
    )
    if (r.action != DefaultAction) table.append(s"action | `${r.action}` ")
    table.append("")
    (table ++ argumentLines).toList
  }

  // Arguments

  /**
    * Create the markdown strings for all the arguments in a rule
    * @param args ArgumentPatterns for the Rule
    * @return markdown table strings
    */
  def argsToMarkdown(args: Seq[ArgumentPattern]): List[String] = {
    if (args.isEmpty) List(
      "",
      "_No arguments_"
    )
    else {
      List(
        "",
        "**argument name** | **label(s)** | **quantifier** | **required?**",
        ":---- | :---- | :---- | :----"
      ) ++ args.map(argToMarkdown)
    }
  }

  /**
    * Create the markdown table line for a given argument, including the name, the label,
    * whether or not the argument was quantified, and if it's optional.
    * @param a
    * @return markdown table string
    */
  def argToMarkdown(a: ArgumentPattern): String = {
    s" ${a.name} | " +
      s"`${a.label}` | " +
      s"${quantifierString(a.quantifier)} | " +
      s"${booleanString(a.required)} "
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

  def toMarkdown(label: String, pairs: Seq[(Rule, Extractor)]): Seq[String] = {
    val (rules, extractors) = pairs.unzip
    assert(pairs.nonEmpty) // Should not happen
    val allRuleNames = listString(rules.map(rule => s"""_${rule.name}_"""))
    val allPriorities = priorityString(extractors.map(_.priority))
    val labels = extractors.head.labels
    // All should have the same labels because they come from the taxonomy
    assert(labels.length == extractors.flatMap(_.labels).distinct.length)
    val allKeep = booleanString(extractors.map(_.keep))
    val allActions = backtickedString(
      rules
        .map(_.action)
        .filterNot(_ == DefaultAction)
    )

    val table = ArrayBuffer[String](
      "----------------------------------",
      "",
      s"###  ${label}",
      "",
      s"|Attribute        |  Value | ",
      s"| :--------       | :---- |",
      s"|rules            | ${allRuleNames} ",
      s"|label hierarchy  | ${labelsString(labels)} ",
      s"|priorities       | ${allPriorities}",
      s"|keep             | ${allKeep} "
    )
    if (allActions.nonEmpty) table.append(s"|actions | ${allActions}")

    val argPatterns = extractors.collect{ case gp: GraphExtractor => gp.pattern.arguments}.flatten
    val argsByName = argPatterns.groupBy(_.name)
    if (argsByName.nonEmpty) {
      table.appendAll(
        List(
          "",
          "_Arguments_",
          "|name        | **label(s)**  | **quantifier(s)** | **required?**|",
          "| :--------  | :----         | :----             | :---- "
        )
      )
      val argLines = for {
        (name, patterns) <- argsByName
        labels = labelsString(patterns.map(_.label).distinct)
        possibleQuantifiers = quantifierString(patterns.map(_.quantifier))
        optionalities = booleanString(patterns.map(_.required))
      } yield s"| _${name}_ | ${labels} | ${possibleQuantifiers} | ${optionalities}"
      table.appendAll(argLines)
    }
    else {
      table.appendAll(List("", "_No arguments_"))
    }
    table.append("")
    table.toList
  }

  def listString(ss: Seq[String]): String = {
    val distinct = ss.distinct.sorted
    distinct.length match {
      case 0 => ""
      case 1 => distinct.head
      case _ => s"[${distinct.mkString(", ")}]"
    }
  }

  def quantifierString(qq: Seq[ArgumentQuantifier]): String =
    listString(qq.map(quantifierString))
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

  def priorityString(pp: Seq[Priority]): String =
    listString(pp.map(priorityString))
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
