package org.clulab.odin.debugger.visualizer.action

import org.clulab.odin.debugger.debug.{FinishedGlobalAction, FinishedLocalAction}
import org.clulab.odin.debugger.visualization.Visualization

import scala.collection.mutable

abstract class ActionVisualizer {
  def visualizeLocal(transcript: Seq[FinishedLocalAction]): Visualization
  def visualizeGlobal(transcript: Seq[FinishedGlobalAction]): Visualization
}
