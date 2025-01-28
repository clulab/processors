package org.clulab.odin.debugger.visualizer

import org.clulab.odin.impl.{CrossSentenceExtractor, Done, Extractor, GraphExtractor, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, SaveEnd, SaveStart, Split, TokenExtractor}

import scala.annotation.tailrec

abstract class Visualizer() {
  def visualize(extractor: Extractor): Unit = {
    extractor match {
      case tokenExtractor: TokenExtractor => visualize(tokenExtractor)
      case graphExtractor: GraphExtractor => visualize(graphExtractor)
      case crossSentenceExtractor: CrossSentenceExtractor => visualize(crossSentenceExtractor)
      case _ => ???
    }
  }

  def visualize(tokenExtractor: TokenExtractor): Unit = ???

  def visualize(graphExtractor: GraphExtractor): Unit = ()

  def visualize(crossSentenceExtractor: CrossSentenceExtractor): Unit = ???
}
