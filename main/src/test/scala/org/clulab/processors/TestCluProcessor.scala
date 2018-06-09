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

  it should "recognize syntactic chunks correctly" in {
    val doc = proc.mkDocument("He reckons the current account deficit will narrow to only 1.8 billion.")
    proc.lemmatize(doc)
    proc.tagPartsOfSpeech(doc)
    proc.chunking(doc)
    doc.clear()

    doc.sentences(0).chunks.get(0) should be ("B-NP")
    doc.sentences(0).chunks.get(1) should be ("B-VP")
    doc.sentences(0).chunks.get(2) should be ("B-NP")
    doc.sentences(0).chunks.get(3) should be ("I-NP")
    doc.sentences(0).chunks.get(4) should be ("I-NP")
    doc.sentences(0).chunks.get(5) should be ("I-NP")
    doc.sentences(0).chunks.get(6) should be ("B-VP")
    doc.sentences(0).chunks.get(7) should be ("I-VP")
    doc.sentences(0).chunks.get(8) should be ("B-PP")
    doc.sentences(0).chunks.get(9) should be ("B-NP")
    doc.sentences(0).chunks.get(10) should be ("I-NP")
    doc.sentences(0).chunks.get(11) should be ("I-NP")
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

    //println("Basic universal dependencies:")
    //println(doc.sentences.head.universalBasicDependencies.get)

    doc.sentences.head.universalBasicDependencies.get.hasEdge(1, 0, "compound") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 1, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 4, "nmod") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 3, "case") should be(true)
    println("Parsing is fine.")
  }

  it should "parse MWEs correctly" in {
    val doc = proc.mkDocument("Foods such as icecream are tasty.")

    println(s"WORDS: ${doc.sentences.head.words.mkString(", ")}")

    proc.annotate(doc)

    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(0, 3, "nmod_such_as") should be (true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(0, 3, "nmod") should be (false)
  }

  /* // TODO
  it should "parse a long sentence correctly" in {
    val doc = proc.annotate("Her T score of 63 on the Attention Problems scale is in the At Risk range suggesting that she sometimes daydreams or is easily distracted and unable to concentrate more than momentarily .")
    //println(s"Sentence: ${doc.sentences(0).words.mkString(" ")}")
    //println("Basic universal dependencies:")
    //println(doc.sentences.head.universalBasicDependencies.get)

    doc.sentences.head.universalBasicDependencies.isDefined should be (true)
    val deps = doc.sentences.head.universalBasicDependencies.get

    (deps.incomingEdges != null) should be (true)
    (deps.outgoingEdges != null) should be (true)

    deps.incomingEdges.length == 33 should be (true)
    deps.outgoingEdges.length == 33 should be (true)

    deps.hasEdge(2, 0, "nmod:poss") should be (true)
    deps.hasEdge(2, 1, "compound") should be (true)
    deps.hasEdge(2, 9, "nmod") should be (true)
  }
  */
}
