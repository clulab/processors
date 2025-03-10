package org.clulab.odin.debugger.visualizer.rule

import org.clulab.odin.debugger.visualization.TextVisualization
import org.clulab.odin.impl.Extractor

class TextRuleVisualizer extends RuleVisualizer {

  def visualize(extractor: Extractor): TextVisualization = {
    val text = extractor.ruleOpt.getOrElse("")

    new TextVisualization(text)
  }
}
