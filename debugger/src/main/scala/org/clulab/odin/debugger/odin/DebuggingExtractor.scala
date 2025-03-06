package org.clulab.odin.debugger.odin

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.impl.{CrossSentenceExtractor, Extractor, GraphExtractor, TokenExtractor}

trait DebuggingExtractor {
  def extractor: Extractor
  def debugger: Debugger

  def withDebuggingChildren: Seq[DebuggingExtractor] = Seq(this)

  def withChildren: Seq[Extractor] = withDebuggingChildren.map(_.extractor)
}

object DebuggingExtractor {

  def apply(extractor: Extractor, debugger: Debugger): Extractor = {
    val debuggingExtractor = extractor match {
      case extractor: TokenExtractor => DebuggingTokenExtractor(debugger, extractor)
      case extractor: GraphExtractor => DebuggingGraphExtractor(debugger, extractor)
      case extractor: CrossSentenceExtractor => DebuggingCrossSentenceExtractor(debugger, extractor)
    }

    debuggingExtractor
  }
/*
  def apply(extractor: Extractor): DebuggingExtractor = {
    val debuggingExtractor = extractor match {
      case extractor: TokenExtractor => DebuggingTokenExtractor(debugger, extractor)
      case extractor: GraphExtractor => DebuggingGraphExtractor(debugger, extractor)
      case extractor: CrossSentenceExtractor => DebuggingCrossSentenceExtractor(debugger, extractor)
    }

    debuggingExtractor
  }*/
}
