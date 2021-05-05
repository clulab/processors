package org.clulab.dynet

import org.clulab.embeddings.WordEmbeddingMapPool
import org.scalatest.{FlatSpec, Matchers}

// The obsolete tests that were previously here have been removed.
// ConstEmbeddingsGlove is an object and is not readily testable.
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
    var embeddings = WordEmbeddingMapPool.getOrElseCreate("/test_vectors")

    0.until(100).foreach { index =>
      val e1 = embeddings.get("time")
      e1.isDefined should be (true)
      e1.get.length should be(5)

      val e2 = embeddings.get("timeout")
      e2.isDefined should be(false)

      val e3 = embeddings.getOrElseUnknown("timeout")
      e3.length should be(5)
    }

    {
      embeddings = null
      // This is for debugging memory leaks.  It will cause other tests to crash.
      // ConstEmbeddingsGlove.SINGLETON = null
    }
  }
}
