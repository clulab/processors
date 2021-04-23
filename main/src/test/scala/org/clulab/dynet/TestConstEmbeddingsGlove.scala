package org.clulab.dynet

import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigValueFactory
import org.scalatest.{FlatSpec, Matchers}

/* FIX ME
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
      val config = ConfigFactory
          .empty
          .withValue("glove.matrixResourceName", ConfigValueFactory.fromAnyRef(s"/test_vectors"))
      ConstEmbeddingsGlove(config)
    }

    0.until(100).foreach { index =>
      var e1 = embeddings.mkEmbedding("time")
      e1 != null should be(true)
      e1.dim().get(0) should be(5)
      e1 = null

      var e2 = embeddings.mkEmbedding("timeout")
      e2 != null should be(true)
      e2.dim().get(0) should be(5)
      e2 = null
    }

    {
      embeddings = null
      // This is for debugging memory leaks.  It will cause other tests to crash.
      // ConstEmbeddingsGlove.SINGLETON = null
    }
  }
}

*/
