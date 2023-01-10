package org.clulab.utils

import java.io.PrintWriter

class TestSerializer extends Test {

  behavior of "Serializer"

  it should "not close a null resource" in {
    val printWriter: PrintWriter = null

    Serializer.using(printWriter) { printWriter =>
      println(printWriter)
    }
  }
}
