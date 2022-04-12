package org.clulab.processors.corenlp.chunker

import java.util.Collection
import edu.stanford.nlp.ie.NERFeatureFactory
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.sequences.Clique
import edu.stanford.nlp.util.PaddedList

class ChunkingFeatureFactory[T <: CoreLabel] extends NERFeatureFactory[T] {
  override def getCliqueFeatures(
      info: PaddedList[T],
      position: Int,
      clique: Clique
  ): Collection[String] = {
    val feats = super.getCliqueFeatures(info, position, clique)
    // add your own custom features here
    // info is the current sentence; the current token is at position
    feats
  }
}
