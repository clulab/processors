package org.clulab.processors

import org.clulab.processors.clu.PortugueseCluProcessor
import org.scalatest.{FlatSpec, Matchers}

class TestPortugueseCluProcessor extends FlatSpec with Matchers {
  val proc = new PortugueseCluProcessor()

  "PortugueseCluProcessor" should "tokenize raw text correctly" in {
    val doc = proc.mkDocument("Da Silva viajou para a China. Lá, ele visitou Pequim.")
    doc.clear()

    doc.sentences(0).words(0) should be ("De")
    doc.sentences(0).words(1) should be ("a")
    doc.sentences(0).words(2) should be ("Silva")
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
    doc.sentences(0).startOffsets(1) should be (1)
    doc.sentences(0).startOffsets(2) should be (3)
    doc.sentences(0).startOffsets(3) should be (9)
    doc.sentences(0).startOffsets(4) should be (16)
    doc.sentences(0).startOffsets(5) should be (21)
    doc.sentences(0).startOffsets(6) should be (23)
    doc.sentences(0).startOffsets(7) should be (28)
    doc.sentences(1).startOffsets(0) should be (30)
    doc.sentences(1).startOffsets(1) should be (32)
    doc.sentences(1).startOffsets(2) should be (34)
    doc.sentences(1).startOffsets(3) should be (38)
    doc.sentences(1).startOffsets(4) should be (46)
    doc.sentences(1).startOffsets(5) should be (52)
  }

  it should "handle valid accents" in {
    val doc = proc.annotate("O deputado João cassou Júnior.")
    doc.sentences.head.words(0) should be ("O")
    doc.sentences.head.words(1) should be ("deputado")
    doc.sentences.head.words(2) should be ("João")
    doc.sentences.head.words(3) should be ("cassou")
    doc.sentences.head.words(4) should be ("Júnior")
    doc.sentences.head.words(5) should be (".")
  }

  it should "handle invalid accents" in {
    val doc = proc.annotate("O deputado Joāo cassou Júnior.")
    doc.sentences.head.words(0) should be ("O")
    doc.sentences.head.words(1) should be ("deputado")
    doc.sentences.head.words(2) should be ("Joao")
    doc.sentences.head.words(3) should be ("cassou")
    doc.sentences.head.words(4) should be ("Júnior")
    doc.sentences.head.words(5) should be (".")
  }

  it should "POS tag correctly" in {
    val doc = proc.mkDocument("Da Silva viajou para a China. Lá, ele visitou Pequim.")
    proc.tagPartsOfSpeech(doc)
    //proc.lemmatize(doc)
    doc.clear()

    doc.sentences(0).tags.get(0) should be ("ADP")
    doc.sentences(0).tags.get(1) should be ("DET")
    doc.sentences(0).tags.get(2) should be ("PROPN")
    doc.sentences(0).tags.get(3) should be ("VERBF")
    doc.sentences(0).tags.get(4) should be ("ADP")
    doc.sentences(0).tags.get(5) should be ("DET")
    doc.sentences(0).tags.get(6) should be ("PROPN")
    doc.sentences(0).tags.get(7) should be ("PUNCT")
    doc.sentences(1).tags.get(0) should be ("ADV")
    doc.sentences(1).tags.get(1) should be ("PUNCT")
    doc.sentences(1).tags.get(2) should be ("PRON")
    doc.sentences(1).tags.get(3) should be ("VERBF")
    doc.sentences(1).tags.get(4) should be ("PROPN")
    doc.sentences(1).tags.get(5) should be ("PUNCT")
  }

  it should "recognize syntactic chunks correctly" in {
    val doc = proc.annotate("Nós descobrimos que a exposição prolongada ao alumínio causa câncer.")

    doc.sentences(0).chunks.get(0) should be ("B-NP")
    doc.sentences(0).chunks.get(1) should be ("B-VP")
    doc.sentences(0).chunks.get(2) should be ("O")
    doc.sentences(0).chunks.get(3) should be ("B-NP")
    doc.sentences(0).chunks.get(4) should be ("I-NP")
    doc.sentences(0).chunks.get(5) should be ("B-VP")
    doc.sentences(0).chunks.get(6) should be ("B-PP")
    doc.sentences(0).chunks.get(7) should be ("B-NP")
    doc.sentences(0).chunks.get(8) should be ("I-NP")
    doc.sentences(0).chunks.get(9) should be ("B-VP")
    doc.sentences(0).chunks.get(10) should be ("B-NP")
    doc.sentences(0).chunks.get(11) should be ("O")
  }

  it should "lemmatize text correctly" in {
    val doc = proc.annotate("Nós descobrimos que a exposição prolongada ao alumínio causa câncer.")

    doc.sentences(0).lemmas.get(0) should be ("nós")
    doc.sentences(0).lemmas.get(1) should be ("descobrir")
    doc.sentences(0).lemmas.get(2) should be ("que")
    doc.sentences(0).lemmas.get(3) should be ("o")
    doc.sentences(0).lemmas.get(4) should be ("exposição")
    doc.sentences(0).lemmas.get(5) should be ("prolongar")
    doc.sentences(0).lemmas.get(6) should be ("a")
    doc.sentences(0).lemmas.get(7) should be ("o")
    doc.sentences(0).lemmas.get(8) should be ("alumínio")
    doc.sentences(0).lemmas.get(9) should be ("causar")
    doc.sentences(0).lemmas.get(10) should be ("câncer")
    doc.sentences(0).lemmas.get(11) should be (".")
  }

  
  // # WRITE A NEW TEST FOR THIS CASE
  it should "parse text correctly" in {
    var doc = proc.annotate("João da Silva viajou para a China.")

    //println("Basic universal dependencies:")
    //println(doc.sentences.head.universalBasicDependencies.get)

    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 0, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(0, 3, "nmod") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 1, "case") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 2, "det") should be(true)

    doc = proc.annotate("Nós descobrimos que a exposição prolongada ao alumínio causa câncer.")
    //println("Basic universal dependencies:")
    //println(doc.sentences.head.universalBasicDependencies.get)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(1, 0, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 5, "acl") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(5, 8, "obl") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(9, 4, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(9, 10, "xcomp") should be(true)

    doc = proc.annotate("Cultivo intensivo de certas culturas será causado pelo encurtamento da disponibilidade do solo.")
    //println("Basic universal dependencies:")
    //println(doc.sentences.head.universalBasicDependencies.get)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(0, 1, "amod") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(0, 4, "nmod") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(6, 0, "nsubj:pass") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(6, 5, "aux:pass") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(6, 9, "obl") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(9, 12, "nmod") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(12, 15, "nmod") should be(true)
  } 
}
