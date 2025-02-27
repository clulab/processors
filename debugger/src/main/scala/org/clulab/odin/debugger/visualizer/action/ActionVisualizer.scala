package org.clulab.odin.debugger.visualizer.action

import org.clulab.odin.debugger.debug.finished.{FinishedGlobalAction, FinishedLocalAction}
import org.clulab.odin.debugger.utils.Transcript
import org.clulab.odin.debugger.visualization.Visualization

import scala.collection.mutable

abstract class ActionVisualizer {
  def visualizeLocal(transcript: Transcript[FinishedLocalAction]): Visualization
  def visualizeGlobal(transcript: Transcript[FinishedGlobalAction]): Visualization
}
