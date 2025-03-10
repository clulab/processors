package org.clulab.odin.debugger.visualizer.rule

import org.clulab.odin.debugger.visualization.Visualization
import org.clulab.odin.impl.Extractor

abstract class RuleVisualizer() {
  def visualize(extractor: Extractor): Visualization
}
