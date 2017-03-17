package org.clulab.processors

import org.clulab.processors.clulab.tokenizer.Tokenizer
import org.scalatest.{FlatSpec, Matchers}

/**
  *
  * User: mihais
  * Date: 3/15/17
  */
class TestTokenizer extends FlatSpec with Matchers {
  "the tokenizer" should "provide fundamental tokenization functionality" in {
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

    sents = tok("gap-mediated reaction")
    sents(0).size should be (2)

    sents = tok("3M is a company")
    sents(0).size should be (4)

    sents = tok("cont'd nat'l program")
    sents(0).size should be (3)

    sents = tok("@user_1 likes #havingfun")
    sents(0).size should be (3)

    sents = tok("h1.cpp is a C++ file")
    sents(0).size should be (5)

    sents = tok("http://google.com is Google's home page")
    sents(0).size should be (6)

    sents = tok("won't isn't hadn't")
    sents(0).size should be (6)
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
