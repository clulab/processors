package org.clulab.odin.debugger.visualizer.inst

import org.clulab.odin.debugger.debug.Transcript
import org.clulab.odin.debugger.debug.finished.FinishedInst
import org.clulab.odin.debugger.visualization.Visualization

abstract class InstVisualizer {

  def visualize(transcript: Transcript[FinishedInst]): Visualization
}
