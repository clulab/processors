package org.clulab.odin.debugger.debugging

import org.clulab.odin.{Action, Mention, State}
import org.clulab.odin.debugger.Debugger
import org.clulab.odin.impl.{CrossSentenceExtractor, Priority, TokenExtractor}
import org.clulab.processors.Document

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

  // This comes indirectly through Extractor.
  override def findAllIn(doc: Document, state: State): Seq[Mention] = debugger.debugExtractor(crossSentenceExtractor) {
    super.findAllIn(doc, state)
  }

  override def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = debugger.debugSentence(sent, doc.sentences(sent)) {
    super.findAllIn(sent, doc, state) // TODO
  }
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
      DebuggingTokenExtractor(debugger, crossSentenceExtractor.anchorPattern),
      DebuggingTokenExtractor(debugger, crossSentenceExtractor.neighborPattern),
      crossSentenceExtractor.anchorRole,
      crossSentenceExtractor.neighborRole
    )
  }
}
