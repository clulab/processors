package org.clulab.processors

import org.clulab.processors.bionlp.BioNLPPreProcessor
import org.scalatest.{Matchers, FlatSpec}

/**
  * Tests the preprocessor
  * User: mihais
  * Date: 8/14/15
  * Last Modified: Add tests for bibliographic reference removal.
 */
class TestBioNLPPreProcessor extends FlatSpec with Matchers {
  val pp = new BioNLPPreProcessor(true, true)
  val ppNoBibRef = new BioNLPPreProcessor(true, false)

  "BioNLPPreProcessor" should "convert Unicode to ASCII" in {
    val s = "\u2714alpha\u03B1\u25B6"
    val ps = pp.preprocess(s)
    ps should be ("valphaalpha>")
  }


  "BioNLPPreProcessor" should "remove simple BIB REF" in {
    val str = "These are known as Kremer bodies (Bernardi and Pandolfi, XREF_BIBR)."
    val rep = "These are known as Kremer bodies                                   ."
    val ppStr = pp.preprocess(str)
    ppStr should be (rep)
  }

  "BioNLPPreProcessor" should "remove BIB REF but leave other paren expression" in {
    val str = "These are referred to as PML nuclear bodies (PML-NBs) but are also known as PML oncogenic domains, nuclear dot 10 or Kremer bodies (Bernardi and Pandolfi, XREF_BIBR)."
    val rep = "These are referred to as PML nuclear bodies (PML-NBs) but are also known as PML oncogenic domains, nuclear dot 10 or Kremer bodies                                   ."
    val ppStr = pp.preprocess(str)
    ppStr should be (rep)
  }

  "BioNLPPreProcessor" should "remove complex BIB REF but leave other paren expressions" in {
    val str = "This tripartite structure contains a RING (really interesting new gene) zinc-finger, two additional zinc-finger motifs (B-box1 and B-box2) and a DUF 3583 domain containing a coiled-coil region (Borden et al., XREF_BIBR; Jensen et al., XREF_BIBR)."
    val rep = "This tripartite structure contains a RING (really interesting new gene) zinc-finger, two additional zinc-finger motifs (B-box1 and B-box2) and a DUF 3583 domain containing a coiled-coil region                                                     ."
    val ppStr = pp.preprocess(str)
    ppStr should be (rep)
  }

  "BioNLPPreProcessor" should "NOT remove simple BIB REF if parameter not true" in {
    val str = "These are known as Kremer bodies (Bernardi and Pandolfi, XREF_BIBR)."
    val ppStr = ppNoBibRef.preprocess(str)
    ppStr should be (str)
  }

}
