package org.clulab.struct

import org.clulab.utils.{StringUtils, Test}

/**
  * Tests Counter methods
  * User: mihais
  * Date: 10/25/16
  */
class TestCounter extends Test {
  "TestCounter" should "serialize content correctly in saveTo " in {
    val string = StringUtils.viaPrintWriter { printWriter =>
      val c = new Counter[String]()
      c += "uno"
      c += "dos"
      c.saveTo(printWriter)
    }

    val eol = System.getProperty("line.separator")
    val content = string.replace(eol, " ")
    val values = content.split(' ')

    val Array(defaultReturnValue, size, kind) = values.take(3)
    defaultReturnValue should be ("0.0")
    size should be ("2")
    kind should be ("S")

    val groups = values.drop(3).sliding(2, 2).toArray
    groups should have size 2
    groups should contain (Array("1.0", "dos"))
    groups should contain (Array("1.0", "uno"))
    // content should be ("0.0 2 S 1.0 dos 1.0 uno ") // The order of this is not certain.
  }
}
