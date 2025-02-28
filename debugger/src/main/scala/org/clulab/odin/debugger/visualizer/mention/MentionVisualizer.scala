package org.clulab.odin.debugger.visualizer.mention

import org.clulab.odin.debugger.debug.Transcript
import org.clulab.odin.debugger.debug.finished.FinishedMention
import org.clulab.odin.debugger.visualization.Visualization

abstract class MentionVisualizer {
  def visualize(transcript: Transcript[FinishedMention]): Visualization
}
