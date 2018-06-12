package org.clulab.processors

import org.clulab.processors.clu.tokenizer.OpenDomainEnglishTokenizer
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
    sents.length should be (2)

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

    sents = tok("so is www.google.com and google.com")
    sents(0).size should be (5)

    sents = tok("won't isn't hadn't")
    sents(0).size should be (6)

    sents = tok("me@privacy.net is a valid email, while me#privacy.net is not.")
    sents(0).size should be (11)

    sents = tok("<start-text>some text</end-text>")
    sents(0).size should be (4)

    sents = tok("IBM is Co. not Ltd.")
    sents(0).size should be (5)

    sents = tok("A.M. Bancorp. A.T.B.") // known abbreviations
    sents(0).size should be (3)

    sents = tok("I'm happy :) not sad :(.")
    sents(0).size should be (8)
  }

  it should "handle contractions correctly" in {
    val sents = tok("I'm won't don't cont'd he's he'd.")
    sents(0).size should be (12)

    sents(0).raw.mkString(" ") should be ("I 'm wo n't do n't cont'd he 's he 'd .")
    sents(0).words.mkString(" ") should be ("I am will not do not cont'd he 's he 'd .")
  }

  it should "transform double quotes into Treebank quotes" in {
    val sents = tok("\"I'm happy\", he said.")
    sents(0).size should be (9)
    sents(0).words(0) should be ("``")
    sents(0).words(4) should be ("''")
  }

  it should "tokenize quotes correctly" in {
    val sent = tok("\"The levels of malnutrition among children continue to be truly alarming,\" said Mahimbo Mdoe, UNICEF's Representative in South Sudan.").head

    sent.raw(0) should be ("\"")
    sent.raw(1) should be ("The")
    sent.raw(12) should be (",")
    sent.raw(13) should be ("\"")
  }

  it should "normalize parentheses correctly" in {
    val sent = tok("This is a test (of parentheses).").head

    sent.raw.length should be (9)
    sent.words(4) should be ("-LRB-")
    sent.words(7) should be ("-RRB-")
  }

  def tok(s:String):Array[Sentence] = {
    println(s"Tokenizing text: $s")
    val t = new OpenDomainEnglishTokenizer(None)
    val sents = t.tokenize(s)
    for(i <- sents.indices) {
      println(s"\tSentence #$i: " + sents(i).words.mkString(", "))
    }
    sents
  }
}
