package org.clulab.dynet

import org.scalatest.{FlatSpec, Matchers}

class TestConstEmbeddingsGlove extends FlatSpec with Matchers {

  try {
    print("dotClassPath = ")
    val classPath = this.getClass.getClassLoader.getResource(".").getPath
    println(classPath)
  }
  catch {
    case _: Throwable => println("unavailable")
  }

  try {
    print("slashClassPath = ")
    val classPath = this.getClass.getClassLoader.getResource("/").getPath
    println(classPath)
  }
  catch {
    case _: Throwable => println("unavailable")
  }

  behavior of "ConstEmbeddingsGlove"

  it should "look realistic" in {
    var embeddings = {
      Utils.initializeDyNet()
      new ConstEmbeddingsGlove("./test_vectors")
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
