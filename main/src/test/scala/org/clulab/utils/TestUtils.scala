package org.clulab.utils

import org.clulab.dynet.Utils

import java.io.FileNotFoundException
import scala.util.Using

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
