package org.clulab.odin.debugger.visualizer.rule

import org.clulab.odin.debugger.utils.RuleUtils
import org.clulab.odin.debugger.visualization.HtmlVisualization
import org.clulab.odin.debugger.visualizer.html.HtmlVisualizing
import org.clulab.odin.impl.Extractor
import scalatags.Text.all._

class HtmlRuleVisualizer extends RuleVisualizer with HtmlVisualizing {

  def visualize(rule: String): Fragment = {
    val scalaRule = RuleUtils.toScalaRule(rule)
    val rows = scalaRule.map { case (key, value) =>
      tr(
        td(`class` := right)(s"$key:"),
        td(toSpans(value.toString))
      )
    }.toSeq

    table(`class` := bordered)(rows)
  }

  def visualize(extractor: Extractor): HtmlVisualization = {
    val fragment = extractor.ruleOpt.map(visualize).getOrElse(
      p(`class` := red)(
        "The rule has not been saved.  Be sure to use OdinConfig.keepRule = true before debugging."
      )
    )

    new HtmlVisualization(fragment)
  }
}
