package org.clulab.embeddings

import org.clulab.utils.HasParallelSupport
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestOverlapWordEmbeddingMap extends AnyFlatSpec with Matchers with HasParallelSupport {

  def overlap(): Unit = {
    val compact = Seq(true, true) // False is no longer available.

    compact.foreach { compact =>
      // Explicit is started first, but compact usually finishes first.
      WordEmbeddingMapPool.getOrElseCreate("/org/clulab/glove/glove.840B.300d.10f", compact)
    }

    toParSeq(compact).reverse.foreach { compact =>
      WordEmbeddingMapPool.getOrElseCreate("/org/clulab/glove/glove.840B.300d.10f", compact)
    }
  }

  behavior of "WordEmbeddingMap"

  it should "overlap" in {
    overlap()
  }
}
