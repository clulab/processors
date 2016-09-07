package org.clulab.discourse.rstparser

import java.io.Writer

import org.clulab.struct.Counter

/**
 * Stores useful stats from the training corpus
 * User: mihais
 * Date: 5/29/14
 */
class CorpusStats (var knownWords:Counter[String],
                   var knownNgrams:Counter[String]) extends Serializable {
  def saveTo(w:Writer) {
    knownWords.saveTo(w)
    knownNgrams.saveTo(w)
  }
}

object CorpusStats {
  def loadFrom[F](r:java.io.Reader):CorpusStats = {
    val words = Counter.loadFrom[String](r)
    val ngrams = Counter.loadFrom[String](r)
    new CorpusStats(words, ngrams)
  }
}
