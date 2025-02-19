package org.clulab.odin.debugger.visualizer.rule

import org.clulab.odin.debugger.visualization.Visualization
import org.clulab.odin.impl.Extractor

import java.io.{PrintWriter, StringWriter}
import scala.util.Using

abstract class RuleVisualizer() {
  def visualize(extractor: Extractor): Visualization
}