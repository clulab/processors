package org.clulab.utils

import org.clulab.scala.Using._

import java.io.PrintWriter

class TestSerializer extends Test {

  behavior of "Serializer"

  it should "not close a null resource" in {
    val printWriter: PrintWriter = null

    assertThrows[NullPointerException] {
      Using.resource(printWriter) { printWriter =>
        println(printWriter)
      }
    }
  }
}
