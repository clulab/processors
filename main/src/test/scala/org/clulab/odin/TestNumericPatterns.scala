package org.clulab.odin

import org.clulab.processors.{Document, Sentence}
import org.clulab.odin.impl.TokenPattern
import org.scalatest.{FlatSpec, Matchers}


class TestNumericPatterns extends FlatSpec with Matchers {

  val text = "blah"
  val doc = Document(
    Array(
      Sentence(
        Array("blah"),
        Array(0),
        Array(4)
      )
    )
  )
  "Any text" should "contain one match for pattern `[2+2==4]`" in {
    val p = TokenPattern.compile("[2+2==4]")
    val state = State(Nil)
    val results = p.findAllIn(0, doc, state)
    results should have size (1)
  }

  "Any text" should "contain no matches for pattern `[2+2==5]`" in {
    val p = TokenPattern.compile("[2+2==5]")
    val state = State(Nil)
    val results = p.findAllIn(0, doc, state)
    results should have size (0)
  }

}
