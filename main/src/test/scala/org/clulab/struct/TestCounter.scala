package org.clulab.struct

import java.io.{BufferedWriter, PrintWriter, StringWriter}
import org.clulab.utils.Files
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
/**
  * Tests Counter methods
  * User: mihais
  * Date: 10/25/16
  */
class TestCounter extends AnyFlatSpec with Matchers {
  "TestCounter" should "serialize content correctly in saveTo " in {
    val sw = new StringWriter()
    val w = Files.toPrintWriter(sw)
    val c = new Counter[String]()
    c += "uno"
    c += "dos"
    c.saveTo(w)
    w.close()

    val eol = System.getProperty("line.separator")
    val content = sw.toString.replace(eol, " ")
    content should be ("0.0 2 S 1.0 dos 1.0 uno ")
  }
}
