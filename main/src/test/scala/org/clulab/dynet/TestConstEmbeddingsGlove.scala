package org.clulab.dynet

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class TestConstEmbeddingsGlove extends FlatSpec with Matchers {

  println("Keith was here")

  val classLoader = this.getClass.getClassLoader
  val result = classLoader.getResourceAsStream("/test_vectors.txt")
  println(result)

  val source = Source.fromFile("./main/src/test/resources/test_vectors.txt")
  println(source)

  "ConstEmbeddingsGlove" should "look realistic" in {
    var embeddings = {
      Utils.initializeDyNet()
      // it worked with ./
      // is there the wrong Resource thing?
      new ConstEmbeddingsGlove("/test_vectors")
    }
    var e = embeddings.mkEmbedding("time")
    e != null should be(true)
    e.dim().get(0) should be(5)
    e = null
    embeddings = null
    // This is for debugging memory leaks.  It will cause other tests to crash.
    // ConstEmbeddingsGlove.SINGLETON = null
  }
}
