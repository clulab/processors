package org.clulab.odin.debugger.debugging

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.{Action, Mention, State}
import org.clulab.odin.impl.{Priority, TokenExtractor, TokenPattern}
import org.clulab.processors.Document

class DebuggingTokenExtractor(
   val debugger: Debugger,
   name: String,
   labels: Seq[String],
   priority: Priority,
   keep: Boolean,
   action: Action,
   pattern: TokenPattern // This may need to be copied
 ) extends TokenExtractor(name, labels, priority, keep, action, pattern) {

  // This comes indirectly through Extractor.
  override def findAllIn(doc: Document, state: State): Seq[Mention] = debugger.debugExtractor(this) {
    super.findAllIn(doc, state)
  }

  override def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = debugger.debugSentence(sent, doc.sentences(sent)) {
    super.findAllIn(sent, doc, state)
  }
}

object DebuggingTokenExtractor {

  def apply(debugger: Debugger, tokenExtractor: TokenExtractor): DebuggingTokenExtractor = {
    new DebuggingTokenExtractor(
      debugger,
      tokenExtractor.name,
      tokenExtractor.labels,
      tokenExtractor.priority,
      tokenExtractor.keep,
      tokenExtractor.action,
      tokenExtractor.pattern
    )
  }
}
