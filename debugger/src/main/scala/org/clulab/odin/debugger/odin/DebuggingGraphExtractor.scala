package org.clulab.odin.debugger.odin

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.{Mention, State}
import org.clulab.odin.impl.{GraphExtractor, GraphPattern, OdinConfig, Priority}
import org.clulab.processors.Document

class DebuggingGraphExtractor(
  val debugger: Debugger,
  val graphExtractor: GraphExtractor,
  name: String,
  labels: Seq[String],
  priority: Priority,
  keep: Boolean,
  debuggingAction: DebuggingAction,
  pattern: GraphPattern,
  config: OdinConfig,
  ruleOpt: Option[String]
) extends GraphExtractor(name, labels, priority, keep, debuggingAction, pattern, config, ruleOpt) with DebuggingExtractor {

  def extractor: GraphExtractor = graphExtractor

  // This comes indirectly through Extractor.
  override def findAllIn(doc: Document, state: State): Seq[Mention] = debugger.debugExtractor(graphExtractor) {
    super.findAllIn(doc, state)
  }

  override def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = debugger.debugSentence(sent, doc.sentences(sent)) {
    super.findAllIn(sent, doc, state)
  }
}

object DebuggingGraphExtractor {

  def apply(debugger: Debugger, graphExtractor: GraphExtractor): DebuggingGraphExtractor = {
    new DebuggingGraphExtractor(
      debugger,
      graphExtractor,
      graphExtractor.name,
      graphExtractor.labels,
      graphExtractor.priority,
      graphExtractor.keep,
      DebuggingAction(debugger, graphExtractor.action, Some(graphExtractor)),
      DebuggingGraphPattern(debugger, graphExtractor.pattern),
      graphExtractor.config,
      graphExtractor.ruleOpt
    )
  }
}
