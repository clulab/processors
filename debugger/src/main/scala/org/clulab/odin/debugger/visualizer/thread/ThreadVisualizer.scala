package org.clulab.odin.debugger.visualizer.thread

import org.clulab.odin.debugger.debug.finished.FinishedThread
import org.clulab.odin.debugger.utils.Transcript
import org.clulab.odin.debugger.visualization.Visualization

abstract class ThreadVisualizer {
  def visualize(transcript: Transcript[FinishedThread]): Visualization
}
