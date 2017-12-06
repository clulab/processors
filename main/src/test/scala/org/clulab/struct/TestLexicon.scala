package org.clulab.struct

import org.scalatest.{Matchers, FlatSpec}

/**
  * Tests Lexicon methods
  * User: dane
  * Date: 12/06/2017
  */
class TestLexicon extends FlatSpec with Matchers {
  "Lexicon" should "add and remove items successfully" in {
    val bort = new Lexicon[String]

    bort.add("pour") // index 0
    bort.add("pour") // no update
    bort.add("pour") // no update
    bort.add("cheesecake") // index 1
    bort.add("pin") // index 2
    bort.add("flour") // index 3
    bort.add("chicken") // index 4

    bort.size should be (5)

    val pinIx = bort.remove("pin")
    pinIx should be (2)

    val nonsenseIx = bort.remove("nonsense")
    nonsenseIx should be (-1)

    bort.get("chicken") should not be empty
    bort.get("chicken").get should be (3) // update indices

    val toRemove = 1
    val noCheesecake = (0 until toRemove).map(i => i -> i) ++ ((toRemove + 1) until bort.size).map(i => i -> (i - 1))
    val alsoBort = bort.mapIndicesTo(noCheesecake.toMap)

    alsoBort.size should be (3)
    alsoBort.get(2) should be ("chicken")
  }
}
