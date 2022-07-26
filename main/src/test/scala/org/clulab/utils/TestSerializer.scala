package org.clulab.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.PrintWriter

class TestSerializer extends AnyFlatSpec with Matchers {

  behavior of "Serializer"

  it should "not close a null resource" in {
    val printWriter: PrintWriter = null

    Serializer.using(printWriter) { printWriter =>
      println(printWriter)
    }
  }
}
