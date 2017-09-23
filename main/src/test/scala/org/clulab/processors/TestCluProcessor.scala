package org.clulab.processors

import org.clulab.processors.clu.CluProcessor
import org.scalatest.{FlatSpec, Matchers}

/**
  * Unit tests for CluProcessor
  * User: mihais
  * Date: 6/17/17
  */
class TestCluProcessor extends FlatSpec with Matchers {
  val proc = new CluProcessor()
  
  "CluProcessor" should "tokenize raw text correctly" in {
    val doc = proc.mkDocument("John Doe went to China. There, he visited Beijing.")
    doc.clear()

    doc.sentences(0).words(0) should be ("John")
    doc.sentences(0).words(1) should be ("Doe")
    doc.sentences(0).words(2) should be ("went")
    doc.sentences(0).words(3) should be ("to")
    doc.sentences(0).words(4) should be ("China")
    doc.sentences(0).words(5) should be (".")
    doc.sentences(1).words(0) should be ("There")
    doc.sentences(1).words(1) should be (",")
    doc.sentences(1).words(2) should be ("he")
    doc.sentences(1).words(3) should be ("visited")
    doc.sentences(1).words(4) should be ("Beijing")
    doc.sentences(1).words(5) should be (".")

    doc.sentences(0).startOffsets(0) should be (0)
    doc.sentences(0).startOffsets(1) should be (5)
    doc.sentences(0).startOffsets(2) should be (9)
    doc.sentences(0).startOffsets(3) should be (14)
    doc.sentences(0).startOffsets(4) should be (17)
    doc.sentences(0).startOffsets(5) should be (22)
    doc.sentences(1).startOffsets(0) should be (24)
    doc.sentences(1).startOffsets(1) should be (29)
    doc.sentences(1).startOffsets(2) should be (31)
    doc.sentences(1).startOffsets(3) should be (34)
    doc.sentences(1).startOffsets(4) should be (42)
    doc.sentences(1).startOffsets(5) should be (49)
    println("Tokenization is fine.")
  }

  it should "POS tag correctly" in {
    val doc = proc.mkDocument("John Doe went to China. There, he visited Beijing.")
    proc.lemmatize(doc)
    proc.tagPartsOfSpeech(doc)
    doc.clear()
    
    doc.sentences(0).tags.get(0) should be ("NNP")
    doc.sentences(0).tags.get(1) should be ("NNP")
    doc.sentences(0).tags.get(2) should be ("VBD")
    doc.sentences(0).tags.get(3) should be ("TO")
    doc.sentences(0).tags.get(4) should be ("NNP")
    doc.sentences(0).tags.get(5) should be (".")
    // doc.sentences(1).tags.get(0) should be ("RB")
    doc.sentences(1).tags.get(1) should be (",")
    doc.sentences(1).tags.get(2) should be ("PRP")
    doc.sentences(1).tags.get(3) should be ("VBD")
    doc.sentences(1).tags.get(4) should be ("NNP")
    doc.sentences(1).tags.get(5) should be (".")
    println("POS tagging is fine.")
  }

  it should "lemmatize text correctly" in {
    val doc = proc.mkDocument("John Doe went to the shops.")
    proc.lemmatize(doc)
    doc.clear()

    doc.sentences(0).lemmas.get(0) should be ("john")
    doc.sentences(0).lemmas.get(2) should be ("go")
    doc.sentences(0).lemmas.get(5) should be ("shop")
    println("Lemmatization is fine.")
  }

  it should "parse text correctly" in {
    val doc = proc.annotate("John Doe went to China")

    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(1, 0, "nn") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 1, "nsubj") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 3, "prep") should be(true)
    doc.sentences.head.stanfordBasicDependencies.get.hasEdge(2, 3, "obj") should be(false)
    println("Parsing is fine.")
  }
}
