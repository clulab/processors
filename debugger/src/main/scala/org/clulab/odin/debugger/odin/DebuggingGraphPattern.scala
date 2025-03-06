package org.clulab.odin.debugger.odin

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.impl.{GraphPattern, RelationGraphPattern, TriggerMentionGraphPattern, TriggerPatternGraphPattern}

trait DebuggingGraphPattern {
  def graphPattern: GraphPattern
  def debugger: Debugger
}

object DebuggingGraphPattern {

  def apply(debugger: Debugger, graphPattern: GraphPattern): GraphPattern = {
    val debuggingGraphPattern = graphPattern match {
      case graphPattern: TriggerPatternGraphPattern => DebuggingTriggerPatternGraphPattern(debugger, graphPattern)
      case graphPattern: TriggerMentionGraphPattern => DebuggingTriggerMentionGraphPattern(debugger, graphPattern)
      case graphPattern: RelationGraphPattern => DebuggingRelationGraphPattern(debugger, graphPattern)
    }

    debuggingGraphPattern
  }
}
