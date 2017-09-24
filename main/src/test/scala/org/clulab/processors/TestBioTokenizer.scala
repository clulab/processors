package org.clulab.processors

import org.clulab.processors.clu.BioCluProcessor
import org.scalatest.{FlatSpec, Matchers}

/**
  * Tests for proper tokenization in the Bio domain
  * User: mihais
  * Date: 9/23/17
  */
class TestBioTokenizer extends FlatSpec with Matchers {
  val proc:Processor = new BioCluProcessor
  val s1 = "Cells were additionally stimulated with 10 ng/ml NRG and cell extracts analyzed for ErbB3 tyrosine phosphorylation"

  "BioCluProcessor" should "NOT tokenize slashes if they are part of measurement units" in {
    val text = "Cells were additionally stimulated with 10 ng/ml NRG and cell extracts analyzed for ErbB3 tyrosine phosphorylation"
    val doc = proc.mkDocument(text, keepText = false)
    proc.annotate(doc)

    val s = doc.sentences(0)
    s.words(6) should be ("ng/ml")
  }

  it should """tokenize ERK/MAPK-signaling into "ERK and MAPK signaling"""" in {
    val text = "Ras activates SAF-1 and MAZ activity through YYY/ZZZ-signaling"
    val doc = proc.mkDocument(text, keepText = false)
    proc.annotate(doc)

    val s = doc.sentences(0)
    s.words(7) should be ("YYY")
    s.words(8) should be ("and")
    s.words(9) should be ("ZZZ")
    s.words(10) should be ("signaling")
  }

  it should "reattach / and tokenize it properly" in {
    var doc = proc.mkDocument("ZZZ-1/YYY-1", keepText = false)
    proc.annotate(doc)

    var s = doc.sentences(0)
    s.words(0) should be ("ZZZ-1")
    s.words(1) should be ("and")
    s.words(2) should be ("YYY-1")

    doc = proc.mkDocument("ERK-1/-2", keepText = false)
    s = doc.sentences(0)
    s.words(0) should be ("ERK-1/-2")

    doc = proc.mkDocument("ERK-1/2", keepText = false)
    s = doc.sentences(0)
    s.words(0) should be ("ERK-1/2")
  }

  it should "NOT tokenize names of protein families around slash" in {
    val doc = proc.mkDocument("EGFR/ERBB or Erk1/3 or MAPK1/MAPK3", keepText = false)
    proc.annotate(doc)

    val s = doc.sentences(0)
    s.words(0) should be ("EGFR/ERBB")
    s.words(2) should be ("Erk1/3")
    s.words(4) should be ("MAPK1/MAPK3")
  }

  it should "tokenize complex names around slash" in {
    val doc = proc.mkDocument("Highly purified DNA-PKcs, Ku70/Ku80 heterodimer and the two documented XRCC1 binding partners LigIII and DNA polbeta were dot-blotted.")
    proc.annotate(doc)

    val s = doc.sentences(0)
    s.words(4) should be ("Ku70")
    s.words(5) should be ("and")
    s.words(6) should be ("Ku80")
  }

  it should "tokenize complex names around dash" in {
    val doc = proc.mkDocument("Highly purified DNA-PKcs, Ku70-Ku80 heterodimer and the two documented XRCC1 binding partners LigIII and DNA polbeta were dot-blotted.")
    proc.annotate(doc)

    val s = doc.sentences(0)
    s.words(4) should be ("Ku70")
    s.words(5) should be ("and")
    s.words(6) should be ("Ku80")
  }

  it should "tokenize n-ary complexes correctly around dash or slash" in {
    var doc = proc.mkDocument("We analyze 4 proteins: 953, ASPP2, Mek, and Ras. We found the p53-ASPP2-Mek-Ras complex.")
    proc.annotate(doc)

    var s = doc.sentences(1)
    s.words(3) should be ("p53")
    s.words(5) should be ("ASPP2")
    s.words(7) should be ("Mek")
    s.words(10) should be ("Ras")

    doc = proc.mkDocument("We found the p53-ASPP2-Smad-2-Ras complex.")
    proc.annotate(doc)

    s = doc.sentences(0)
    s.words(3) should be ("p53")
    s.words(5) should be ("ASPP2")
    s.words(7) should be ("Smad-2")
    s.words(10) should be ("Ras")

    doc = proc.mkDocument("We found the p53-ASPP2-Smad-2-Smad-3 complex.")
    proc.annotate(doc)

    s = doc.sentences(0)
    s.words(3) should be ("p53")
    s.words(5) should be ("ASPP2")
    s.words(7) should be ("Smad-2")
    s.words(10) should be ("Smad-3")

    doc = proc.mkDocument("We found the p53/ASPP2/Smad2/Smad3 complex.")
    proc.annotate(doc)

    s = doc.sentences(0)
    s.words(3) should be ("p53")
    s.words(5) should be ("ASPP2")
    s.words(7) should be ("Smad2")
    s.words(10) should be ("Smad3")

    doc = proc.mkDocument("We found the p53/ASPP2/Smad-2/Smad-3 complex.")
    proc.annotate(doc)

    s = doc.sentences(0)
    s.words(3) should be ("p53")
    s.words(5) should be ("ASPP2")
    s.words(7) should be ("Smad-2")
    s.words(10) should be ("Smad-3")

    doc = proc.mkDocument("We found the p53/ASPP2/Smad-2-Smad-3 complex.")
    proc.annotate(doc)

    s = doc.sentences(0)
    s.words(3) should be ("p53")
    s.words(5) should be ("ASPP2")
    s.words(7) should be ("Smad-2")
    s.words(10) should be ("Smad-3")
  }

  it should "not tokenize up, down, re" in {
    var doc = proc.mkDocument("up-regulation")
    proc.annotate(doc)
    var s = doc.sentences(0)
    s.words(0) should be ("up-regulation")

    doc = proc.mkDocument("down-regulation")
    proc.annotate(doc)
    s = doc.sentences(0)
    s.words(0) should be ("down-regulation")

    doc = proc.mkDocument("re-regulation")
    proc.annotate(doc)
    s = doc.sentences(0)
    s.words(0) should be ("re-regulation")
  }

  // TODO: add tests for the tokenization of mutations - DANE
}
