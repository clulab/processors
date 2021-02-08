package org.clulab.dynet

import org.scalatest.{FlatSpec, Matchers}

class TestConstEmbeddingsGlove extends FlatSpec with Matchers {

  "ConstEmbeddingsGlove" should "look realistic" in {
    var embeddings = {
      Utils.initializeDyNet()
      ConstEmbeddingsGlove("/test_vectors.txt", true)
    }
    var e = embeddings.get("time")
    e != null should be(true)
    e.dim().get(0) should be(5)
    e = null
    embeddings = null
    // This is for debugging memory leaks.  It will cause other tests to crash.
    // ConstEmbeddingsGlove.SINGLETON = null
  }
}
