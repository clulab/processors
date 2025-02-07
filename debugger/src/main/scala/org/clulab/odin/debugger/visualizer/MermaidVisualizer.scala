package org.clulab.odin.debugger.visualizer

import org.clulab.odin.impl.Extractor

class MermaidVisualization() extends Visualization {

  override def toString: String = ""
}

class MermaidVisualizer() extends Visualizer() {

  override def visualize(extractor: Extractor): MermaidVisualization = ???
}
