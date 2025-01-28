package org.clulab.odin.debugger.visualizer

import org.clulab.odin.impl.{CrossSentenceExtractor, Done, Extractor, GraphExtractor, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, SaveEnd, SaveStart, Split, TokenExtractor}

import scala.annotation.tailrec

abstract class Visualizer() {
  def visualize(extractor: Extractor): Unit = ()
}
