package org.clulab.odin.debugger.visualizer.thread

import org.clulab.odin.debugger.debug.FinishedThread
import org.clulab.odin.debugger.visualization.Visualization

import scala.collection.mutable

abstract class ThreadVisualizer {
  def visualize(transcript: mutable.Buffer[FinishedThread]): Visualization
}
