package org.clulab.processors

import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.scalatest.{FlatSpec, Matchers}

class TestDocDateAnnotation extends FlatSpec with Matchers {
  val proc = new ShallowNLPProcessor()

  "ShallowNLPProcessor" should "normalize relative dates correctly when DCT is provided" in {
    val doc = proc.mkDocument("The party that took place last week, on March 15, was awesome.")
    doc.setDCT("2020-03-20")
    proc.annotate(doc)

    val s = doc.sentences(0)
    val ents = s.entities.get
    val norms = s.norms.get

    println(ents.mkString(", "))
    println(norms.mkString(", "))

    ents.length should be(15)
    norms.length should be(15)

    ents(5) should be ("DATE")
    ents(6) should be ("DATE")
    ents(9) should be ("DATE")
    ents(10) should be ("DATE")

    norms(5) should be ("2020-W11")
    norms(9) should be ("2020-03-15")
  }
}
