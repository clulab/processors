package org.clulab.utils

import org.scalatest.{FlatSpec, Matchers}

import java.io.PrintWriter

class TestSerializer extends FlatSpec with Matchers {

  behavior of "Serializer"

  it should "not close a null resource" in {
    val printWriter: PrintWriter = null

    Serializer.using(printWriter) { printWriter =>
      println(printWriter)
    }
  }
}
