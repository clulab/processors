package org.clulab.odin.debugger.visualizer.action

import org.clulab.odin.debugger.debug.{FinishedGlobalAction, FinishedLocalAction}
import org.clulab.odin.debugger.visualization.Visualization

import scala.collection.mutable

abstract class ActionVisualizer {
  def visualizeLocal(transcript: mutable.Buffer[FinishedLocalAction]): Visualization
  def visualizeGlobal(transcript: mutable.Buffer[FinishedGlobalAction]): Visualization
}
