package org.clulab.odin.debugger.odin

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.{Action, Mention, State}
import org.clulab.odin.impl.{Extractor, Priority, TokenExtractor, TokenPattern}
import org.clulab.processors.Document

class DebuggingTokenExtractor(
   override val debugger: Debugger,
   val tokenExtractor: TokenExtractor,
   name: String,
   labels: Seq[String],
   priority: Priority,
   keep: Boolean,
   action: Action,
   pattern: TokenPattern,
   ruleOpt: Option[String]
 ) extends TokenExtractor(name, labels, priority, keep, action, pattern, ruleOpt) with DebuggingExtractor {

  def extractor: TokenExtractor = tokenExtractor

  // This comes indirectly through Extractor.
  override def findAllIn(doc: Document, state: State): Seq[Mention] = debugger.debugExtractor(tokenExtractor) {
    debugger.debugTokenPattern(tokenExtractor.pattern) {
      super.findAllIn(doc, state)
    }
  }

  override def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = debugger.debugSentence(sent, doc.sentences(sent)) {
    super.findAllIn(sent, doc, state)
  }
}

object DebuggingTokenExtractor {

  def apply(debugger: Debugger, tokenExtractor: TokenExtractor, ruleOpt: Option[String] = None): DebuggingTokenExtractor = {
    new DebuggingTokenExtractor(
      debugger,
      tokenExtractor,
      tokenExtractor.name,
      tokenExtractor.labels,
      tokenExtractor.priority,
      tokenExtractor.keep,
      DebuggingAction(debugger, tokenExtractor.action, Some(tokenExtractor)),
      DebuggingTokenPattern(debugger, tokenExtractor.pattern),
      ruleOpt.orElse(tokenExtractor.ruleOpt)
    )
  }
}
