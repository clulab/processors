package org.clulab.dynet

import org.scalatest.{FlatSpec, Matchers}

class TestConstEmbeddingsGlove extends FlatSpec with Matchers {

  lazy val embeddings = {
    Utils.initializeDyNet(train = false)
    ConstEmbeddingsGlove("/test_vectors.txt", true)
  }

  "ConstEmbeddingsGlove" should "not be empty" in {
    val e = embeddings.get("time")
    e != null should be(true)
    e.dim().get(0) should be (5)
  }

}
