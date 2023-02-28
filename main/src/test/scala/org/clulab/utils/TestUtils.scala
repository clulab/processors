package org.clulab.utils

import org.clulab.dynet.Utils
import org.clulab.scala.Using._

import java.io.FileNotFoundException

class TestUtils extends Test {

  behavior of "Utils"

  it should "not create a non-existent source" in {
    assertThrows[FileNotFoundException] {
      val source = Utils.newSource("missing")

      Using.resource(source) { source =>
        println(source)
      }
    }
  }
}
