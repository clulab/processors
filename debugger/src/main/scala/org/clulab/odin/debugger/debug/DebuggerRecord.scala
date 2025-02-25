package org.clulab.odin.debugger.debug

import org.clulab.odin.impl.{Extractor, TokenPattern}
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Interval

trait DebuggerRecordTrait {
  // Include the various ways to filter including for ways that don't exist?

}

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
) extends DebuggerRecordTrait

case class DebuggerRecordForInst()

case class DebuggerRecordForThread()

case class DebuggerRecordForLocalAction(
  document: Document,
  loop: Int,
  extractor: Extractor,
//  tokenPattern: TokenPattern, // Some of the extractors don't have this.
  sentenceIndex: Int,
  sentence: Sentence
)

case class DebuggerRecordForGlobalAction(
  document: Document,
  loop: Int
)

case class DebuggerRecordForMention(
  document: Document,
  loop: Int,
  extractor: Extractor,
  sentenceIndex: Int,
  sentence: Sentence
)
