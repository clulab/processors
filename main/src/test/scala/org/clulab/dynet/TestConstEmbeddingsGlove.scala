package org.clulab.dynet

import org.scalatest.{FlatSpec, Matchers}

class TestConstEmbeddingsGlove extends FlatSpec with Matchers {

  // TODO: this doesn't work when tests are run in parallel because the ConstEmbeddingsGlove might be created by a different test
  /*
  lazy val embeddings = {
    Utils.initializeDyNet()
    ConstEmbeddingsGlove("/test_vectors.txt", true)
  }

  "ConstEmbeddingsGlove" should "not be empty" in {
    val e = embeddings.get("time")
    e != null should be(true)
    e.dim().get(0) should be (5)
  }
  */
}
