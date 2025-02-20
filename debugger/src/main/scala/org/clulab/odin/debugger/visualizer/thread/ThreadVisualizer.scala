package org.clulab.odin.debugger.visualizer.thread

import org.clulab.odin.debugger.debug.FinishedThread
import org.clulab.odin.debugger.visualization.Visualization

abstract class ThreadVisualizer {
  def visualize(transcript: Seq[FinishedThread]): Visualization
}
