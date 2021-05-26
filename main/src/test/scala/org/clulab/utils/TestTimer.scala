package org.clulab.utils

import org.scalatest._

class TestTimer extends FlatSpec with Matchers {

  behavior of "timer"

  it should "use the right divisors" in {
    val timer = new Timer("testing")

    timer.elapsedToString() should be ("0:00:00:00.000")
    timer.elapsedTime += 1000000L
    timer.elapsedToString() should be ("0:00:00:00.001")
    timer.elapsedTime += 1000L * 1000000
    timer.elapsedToString() should be ("0:00:00:01.001")
    timer.elapsedTime += 60L * 1000 * 1000000
    timer.elapsedToString() should be ("0:00:01:01.001")
    timer.elapsedTime += 60L * 60 * 1000 * 1000000
    timer.elapsedToString() should be ("0:01:01:01.001")
    timer.elapsedTime += 24L * 60 * 60 * 1000 * 1000000
    timer.elapsedToString() should be ("1:01:01:01.001")
    timer.elapsedTime += 24L * 60 * 60 * 1000 * 1000000
    timer.elapsedToString() should be ("2:01:01:01.001")
  }
}
