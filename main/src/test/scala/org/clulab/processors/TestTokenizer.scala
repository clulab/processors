package org.clulab.processors

import org.clulab.processors.clulab.tokenizer.Tokenizer
import org.scalatest.{FlatSpec, Matchers}

/**
  *
  * User: mihais
  * Date: 3/15/17
  */
class TestTokenizer extends FlatSpec with Matchers {
  "the tokenizer" should "tokenize sentences correctly" in {
    val s1 = "the of $><1/2 1.3 another test."
    Tokenizer.tokenize(s1)(0).size should be (10)

    
  }
}
