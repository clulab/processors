package org.clulab.utils

import org.clulab.dynet.Utils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.FileNotFoundException

class TestUtils extends AnyFlatSpec with Matchers {

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
