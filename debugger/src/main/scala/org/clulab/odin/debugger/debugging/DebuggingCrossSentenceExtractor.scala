package org.clulab.odin.debugger.debugging

import org.clulab.odin.Action
import org.clulab.odin.debugger.Debugger
import org.clulab.odin.impl.{CrossSentenceExtractor, Priority, TokenExtractor}

class DebuggingCrossSentenceExtractor(
  val debugger: Debugger,
  val crossSentenceExtractor: CrossSentenceExtractor,
  name: String,
  labels: Seq[String],
  priority: Priority,
  keep: Boolean,
  action: Action,
  leftWindow: Int,
  rightWindow: Int,
  anchorPattern: TokenExtractor,
  neighborPattern: TokenExtractor,
  anchorRole: String,
  neighborRole: String
) extends CrossSentenceExtractor(name, labels, priority, keep, action, leftWindow, rightWindow, anchorPattern,
    neighborPattern, anchorRole, neighborRole) {

}

object DebuggingCrossSentenceExtractor {

  def apply(debugger: Debugger, crossSentenceExtractor: CrossSentenceExtractor): DebuggingCrossSentenceExtractor = {
    new DebuggingCrossSentenceExtractor(
      debugger,
      crossSentenceExtractor,
      crossSentenceExtractor.name,
      crossSentenceExtractor.labels,
      crossSentenceExtractor.priority,
      crossSentenceExtractor.keep,
      crossSentenceExtractor.action,
      crossSentenceExtractor.leftWindow,
      crossSentenceExtractor.rightWindow,
      crossSentenceExtractor.anchorPattern,
      crossSentenceExtractor.neighborPattern,
      crossSentenceExtractor.anchorRole,
      crossSentenceExtractor.neighborRole
    )
  }
}
