package org.clulab.odin.debugger.debugging

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.{Action, Mention, State}
import org.clulab.odin.impl.{Priority, TokenExtractor, TokenPattern}
import org.clulab.processors.Document

class DebuggingTokenExtractor(
   val debugger: Debugger,
   val tokenExtractor: TokenExtractor,
   name: String,
   labels: Seq[String],
   priority: Priority,
   keep: Boolean,
   action: Action,
   pattern: TokenPattern
 ) extends TokenExtractor(name, labels, priority, keep, action, pattern) {

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

  def apply(debugger: Debugger, tokenExtractor: TokenExtractor): DebuggingTokenExtractor = {
    new DebuggingTokenExtractor(
      debugger,
      tokenExtractor,
      tokenExtractor.name,
      tokenExtractor.labels,
      tokenExtractor.priority,
      tokenExtractor.keep,
      tokenExtractor.action,
      DebuggingTokenPattern(debugger, tokenExtractor.pattern)
    )
  }
}
