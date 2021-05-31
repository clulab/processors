package org.clulab.embeddings

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TestOverlapWordEmbeddingMap extends FlatSpec with Matchers {

  def overlap(): Unit = {
    val compact = Seq(true, true) // False is no longer available.

    compact.foreach { compact =>
      // Explicit is started first, but compact usually finishes first.
      WordEmbeddingMapPool.getOrElseCreate("/org/clulab/glove/glove.840B.300d.10f", compact)
    }

    compact.par.reverse.foreach { compact =>
      WordEmbeddingMapPool.getOrElseCreate("/org/clulab/glove/glove.840B.300d.10f", compact)
    }
  }

  behavior of "WordEmbeddingMap"

  it should "overlap" in {
    overlap()
  }
}
