package org.clulab.odin.debugger.visualizer.sentence

import org.clulab.odin.debugger.visualization.Visualization
import org.clulab.processors.Sentence

abstract class SentenceVisualizer() {

  def visualize(sentence: Sentence): Visualization
}
