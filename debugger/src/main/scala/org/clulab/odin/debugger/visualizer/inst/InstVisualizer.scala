package org.clulab.odin.debugger.visualizer.inst

import org.clulab.odin.debugger.FinishedInst
import org.clulab.odin.debugger.visualization.Visualization

import scala.collection.mutable

abstract class InstVisualizer {

  def visualize(transcript: mutable.Buffer[FinishedInst]): Visualization
}
