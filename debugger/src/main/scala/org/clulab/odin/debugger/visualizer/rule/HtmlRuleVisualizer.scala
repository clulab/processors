package org.clulab.odin.debugger.visualizer.rule

import org.clulab.odin.debugger.visualization.HtmlVisualization
import org.clulab.odin.debugger.visualizer.html.HtmlVisualizing
import org.clulab.odin.impl.Extractor
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor
import scalatags.Text.all._
import scalatags.generic.Frag
import scalatags.text.Builder

import java.util.{Map => JMap}
import scala.jdk.CollectionConverters._

class HtmlRuleVisualizer extends RuleVisualizer with HtmlVisualizing {
  val yaml = new Yaml(new Constructor(classOf[JMap[String, Any]]))

  def visualize(rule: String): Fragment = {
    val longMap = yaml.load(rule).asInstanceOf[JMap[String, Any]].asScala
    val rows = longMap.map { case (key, value) =>
      tr(
        td(`class` := right)(s"$key:"),
        td(toSpans(value.toString))
      )
    }.toSeq

    table(`class` := bordered)(rows)
  }

  def visualize(extractor: Extractor): HtmlVisualization = {
    val fragment = extractor.ruleOpt.map(visualize).getOrElse(frag())

    new HtmlVisualization(fragment)
  }
}
