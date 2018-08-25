package edu.cmu.dynet.examples

import org.scalatest._

class TestXorScala extends FlatSpec with Matchers {

  behavior of "XorScala"

  it should "not throw an exception" in {
    noException should be thrownBy {
      XorScala.main(Array[String]())
   }
  }
}
