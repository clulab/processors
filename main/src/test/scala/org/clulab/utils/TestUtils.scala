package org.clulab.utils

import org.clulab.dynet.Utils
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.FileNotFoundException

class TestUtils extends FlatSpec with Matchers {

  behavior of "Utils"

  it should "not create a non-existent source" in {
    assertThrows[FileNotFoundException] {
      val source = Utils.newSource("missing")

      Serializer.using(source) { source =>
        println(source)
      }
    }
  }
}
