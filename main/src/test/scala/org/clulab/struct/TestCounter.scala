package org.clulab.struct

import java.io.{BufferedWriter, PrintWriter, StringWriter}

import org.clulab.utils.Files
import org.scalatest.{Matchers, FlatSpec}

/**
  * Tests Counter methods
  * User: mihais
  * Date: 10/25/16
  */
class TestCounter extends FlatSpec with Matchers {
  "TestCounter" should "serialize content correctly in saveTo " in {
    val sw = new StringWriter()
    val w = Files.toPrintWriter(sw)
    val c = new Counter[String]()
    c += "uno"
    c += "dos"
    c.saveTo(w)
    w.close()

    val content = sw.toString.replaceAll("\n", " ")
    content should be ("0.0 2 S 1.0 dos 1.0 uno ")
  }
}
