package org.clulab.processors

import org.clulab.processors.clu.PortugueseCluProcessor
import org.scalatest.{FlatSpec, Matchers}

class TestPortugueseCluProcessor extends FlatSpec with Matchers {
  val proc = new PortugueseCluProcessor()

  "PortugueseCluProcessor" should "tokenize raw text correctly" in {
    val doc = proc.mkDocument("Fulano de Tal viajou para a China. Lá, ele visitou Pequim.")
    doc.clear()

    doc.sentences(0).words(0) should be ("Fulano")
    doc.sentences(0).words(1) should be ("de")
    doc.sentences(0).words(2) should be ("Tal")
    doc.sentences(0).words(3) should be ("viajou")
    doc.sentences(0).words(4) should be ("para")
    doc.sentences(0).words(5) should be ("a")
    doc.sentences(0).words(6) should be ("China")
    doc.sentences(0).words(7) should be (".")
    doc.sentences(1).words(0) should be ("Lá")
    doc.sentences(1).words(1) should be (",")
    doc.sentences(1).words(2) should be ("ele")
    doc.sentences(1).words(3) should be ("visitou")
    doc.sentences(1).words(4) should be ("Pequim")
    doc.sentences(1).words(5) should be (".")

    doc.sentences(0).startOffsets(0) should be (0)
    doc.sentences(0).startOffsets(1) should be (7)
    doc.sentences(0).startOffsets(2) should be (10)
    doc.sentences(0).startOffsets(3) should be (14)
    doc.sentences(0).startOffsets(4) should be (21)
    doc.sentences(0).startOffsets(5) should be (26)
    doc.sentences(0).startOffsets(6) should be (28)
    doc.sentences(0).startOffsets(7) should be (33)
    doc.sentences(1).startOffsets(0) should be (35)
    doc.sentences(1).startOffsets(1) should be (37)
    doc.sentences(1).startOffsets(2) should be (39)
    doc.sentences(1).startOffsets(3) should be (43)
    doc.sentences(1).startOffsets(4) should be (51)
    doc.sentences(1).startOffsets(5) should be (57)
    println("Tokenization is fine.")
  }

  it should "POS tag correctly" in {
    val doc = proc.mkDocument("Fulano de Tal viajou para a China. Lá, ele visitou Pequim.")
    proc.lemmatize(doc)
    proc.tagPartsOfSpeech(doc)
    doc.clear()

    doc.sentences(0).tags.get(0) should be ("PROPN")
    doc.sentences(0).tags.get(1) should be ("ADP")
    doc.sentences(0).tags.get(2) should be ("PROPN")
    doc.sentences(0).tags.get(3) should be ("VERB")
    doc.sentences(0).tags.get(4) should be ("ADP")
    doc.sentences(0).tags.get(5) should be ("DET")
    doc.sentences(0).tags.get(6) should be ("PROPN")
    doc.sentences(0).tags.get(7) should be ("PUNCT")
    doc.sentences(1).tags.get(0) should be ("ADP")
    doc.sentences(1).tags.get(1) should be ("PUNCT")
    doc.sentences(1).tags.get(2) should be ("PRON")
    doc.sentences(1).tags.get(3) should be ("VERB")
    doc.sentences(1).tags.get(4) should be ("PROPN")
    doc.sentences(1).tags.get(5) should be ("PUNCT")
    println("POS tagging is fine.")
  }

/*
  it should "recognize syntactic chunks correctly" in {
    val doc = proc.mkDocument("")
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
