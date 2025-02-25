package org.clulab.odin.debugger.visualizer.mention

import org.clulab.odin.debugger.debug.FinishedMention
import org.clulab.odin.debugger.visualization.Visualization

abstract class MentionVisualizer {
  def visualize(transcript: Seq[FinishedMention]): Visualization
}
