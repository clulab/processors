package org.clulab.odin.debugger.debug

import org.clulab.odin.impl.{Extractor, TokenPattern}
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Interval

// Turn into SearchRecord, InstRecord, ThreadRecord
case class DebuggerRecord(
  // TODO: Maybe include depth?
  document: Document,
  loop: Int,
  extractor: Extractor,

  tokenPattern: TokenPattern,
  sentenceIndex: Int,
  sentence: Sentence,

  startOpt: Option[Int] = None,
  tokOpt: Option[Int] = None, // TODO: Maybe skip
  tokenIntervalOpt: Option[Interval] = None
)
