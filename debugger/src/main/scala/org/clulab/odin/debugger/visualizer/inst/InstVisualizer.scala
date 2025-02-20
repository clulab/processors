package org.clulab.odin.debugger.visualizer.inst

import org.clulab.odin.debugger.debug.FinishedInst
import org.clulab.odin.debugger.visualization.Visualization

abstract class InstVisualizer {

  def visualize(transcript: Seq[FinishedInst]): Visualization
}
