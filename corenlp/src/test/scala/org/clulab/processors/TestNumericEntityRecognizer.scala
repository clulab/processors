package org.clulab.processors

import org.clulab.processors.clucore.NumericEntityRecognizer
import org.scalatest.{FlatSpec, Matchers}

class TestNumericEntityRecognizer extends FlatSpec with Matchers {
  val ner = new NumericEntityRecognizer

  "NumericEntityRecognizer" should "recognize dates" in {
    val (labels, norms) = ner.classify(
      Array("He", "was", "born", "between", "January", "1st", ",", "1966", "and", "1997", "."),
      Array("PRP", "VBD", "VBN", "IN", "NNP", "NN", ",", "CD", "CC", "CD", "."),
      Array(0, 3, 7, 12, 20, 28, 32, 34, 39, 43, 48),
      Array(2, 6, 11, 19, 27, 31, 33, 38, 42, 47, 49),
      None
    )
    println(labels.mkString(", "))
    println(norms.mkString(", "))

    labels(4) should be ("DATE")
    labels(5) should be ("DATE")
    labels(6) should be ("DATE")
    labels(7) should be ("DATE")
    labels(8) should be ("O")
    labels(9) should be ("DATE")

    norms(4) should be ("1966-01-01")
    norms(9) should be ("1997")
  }

  it should "recognize date ranges" in {
    val (labels, norms) = ner.classify(
      Array("He", "was", "born", "between", "1996", "and", "1997", "."), // He was born between 1996 and 1997 .
      Array("PRP", "VBD", "VBN", "IN", "CD", "CC", "CD", "."), // PRP VBD VBN IN CD CC CD .
      Array(0, 3, 7, 12, 20, 25, 29, 33),
      Array(2, 6, 11, 19, 24, 28, 33, 34),
      None
    )
    println(labels.mkString(", "))
    println(norms.mkString(", "))

    labels(4) should be ("DATE")
    labels(5) should be ("DATE")
    labels(6) should be ("DATE")

    norms(4) should be ("1996/1997")
  }
}
