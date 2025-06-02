package org.clulab.odin

import org.clulab.processors.{Document, Sentence}
import org.clulab.odin.impl.TokenPattern
import org.clulab.utils.Test


class TestNumericPatterns extends Test {

  val text = "blah"
  val doc = Document(
    Seq(
      Sentence(
        Seq("blah"),
        Seq(0),
        Seq(4),
        Seq("blah")
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
