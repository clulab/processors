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
    var sents = tok("the of $><1/2 1.3 another test.")
    sents(0).size should be (10)

    sents = tok("-LRB- -RRB- -LCB- -RCB- -LSB- -RSB-.")
    sents(0).size should be (7)

    sents = tok("first sentence. second sentence.")
    sents.size should be (2)

    sents = tok("today is 12/25/2017")
    sents(0).size should be (3)
    sents = tok("today is 12/255/2017")
    sents(0).size > 3 should be (true)

    sents = tok("number is -122,333")
    sents(0).size should be (3)
  }

  def tok(s:String):Array[Sentence] = {
    println(s"Tokenizing text: $s")
    val sents = Tokenizer.tokenize(s)
    for(i <- sents.indices) {
      println(s"\tSentence #${i}: " + sents(i).words.mkString(", "))
    }
    sents
  }
}
