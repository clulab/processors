package org.clulab.embeddings

import org.clulab.utils.Test
import org.clulab.utils.ThreadUtils

class TestOverlapWordEmbeddingMap extends Test {

  def overlap(): Unit = {
    val compact = Seq(true, true) // False is no longer available.

    compact.foreach { compact =>
      // Explicit is started first, but compact usually finishes first.
      WordEmbeddingMapPool.getOrElseCreate("/org/clulab/glove/glove.840B.300d.10f", compact)
    }

    ThreadUtils.parallelize(compact.reverse, compact.length).foreach { compact =>
      WordEmbeddingMapPool.getOrElseCreate("/org/clulab/glove/glove.840B.300d.10f", compact)
    }
  }

  behavior of "WordEmbeddingMap"

  it should "overlap" in {
    overlap()
  }
}
