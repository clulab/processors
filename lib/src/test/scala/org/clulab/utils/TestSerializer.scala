package org.clulab.utils

import java.io.PrintWriter
import scala.util.Using

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
