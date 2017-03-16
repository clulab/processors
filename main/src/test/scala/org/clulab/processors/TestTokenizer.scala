package org.clulab.processors

import org.clulab.processors.clulab.tokenizer.Tokenizer
import org.scalatest.{FlatSpec, Matchers}

/**
  *
  * User: mihais
  * Date: 3/15/17
  */
class TestTokenizer extends FlatSpec with Matchers {
  "the tokenizer" should "tokenizer correctly the sentence" in {
    val s = "the of $ ."
    (new Tokenizer).tokenize(s)
  }
}
