package org.clulab.utils

import org.clulab.utils.Ops._

import scala.util.Try

class TestOps extends Test {

  behavior of "ObjectOps"

  it should "convert instances" in {
    val example1: Any = 42
    val expectedResult1 = Try { example1.asInstanceOf[String] }.toOption
    val actualResult1 = example1.asInstanceOfOption[String]
    val usage1 = example1.asInstanceOfOption[String].getOrElse("UNK")

    actualResult1 should be (expectedResult1)

    val example2: Any = "forty-two"
    val expectedResult2 = Try { example2.asInstanceOf[Int] }.toOption
    val actualResult2 = example2.asInstanceOfOption[Int]
    val usage2 = example2.asInstanceOfOption[Int].getOrElse(-1)

    actualResult2 should be (expectedResult2)
  }
}
