package org.clulab.odin.debugger.visualization

class HtmlVisualization(val fragment: HtmlFragment.Fragment) extends Visualization {

  override def toString: String = fragment.toString
}
