package org.clulab.odin.visualizer

import org.clulab.odin.impl.{CrossSentenceExtractor, Done, Extractor, GraphExtractor, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, SaveEnd, SaveStart, Split, TokenExtractor}

import scala.annotation.tailrec

abstract class Visualizer() {
  def visualize(extractor: Extractor): Unit = {
    extractor match {
      case tokenExtractor: TokenExtractor => visualizeTokenExtractor(tokenExtractor)
      case graphExtractor: GraphExtractor => visualizeGraphExtractor(graphExtractor)
      case crossSentenceExtractor: CrossSentenceExtractor => visualizeCrossSentenceExtractor(crossSentenceExtractor)
      case _ => ???
    }
  }

  def visualizeTokenExtractor(tokenExtractor: TokenExtractor): Unit = ???

  def visualizeGraphExtractor(graphExtractor: GraphExtractor): Unit = ()

  def visualizeCrossSentenceExtractor(crossSentenceExtractor: CrossSentenceExtractor): Unit = ???
}
