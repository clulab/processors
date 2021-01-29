package org.clulab.dynet

import org.clulab.FatdynetTest

class TestConstEmbeddingsGlove extends FatdynetTest {

  "ConstEmbeddingsGlove" should "not be empty" in {
    var embeddings = {
      Utils.initializeDyNet()
      ConstEmbeddingsGlove("/test_vectors.txt", true)
    }

    var e = embeddings.get("time")
    e != null should be(true)
    e.dim().get(0) should be (5)
    e = null
    embeddings = null
    ConstEmbeddingsGlove.SINGLETON = null
  }
}
