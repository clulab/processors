package org.clulab.odin.debugger.visualization

import org.clulab.odin.debugger.visualizer.HtmlVisualizing
import scalatags.generic.Frag
import scalatags.text.Builder

class HtmlVisualization(val fragment: Frag[Builder, String]) extends Visualization {

  override def toString: String = fragment.toString
}
