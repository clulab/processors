package edu.arizona.sista.odin.domains.bigmechanism.dryrun2015

import org.scalatest._
import TestResources._
import DarpaEvalUtils._

/**
 * Unit tests to make sure we do not over predict
 */
class TestFalsePositive extends FlatSpec with Matchers {
  val sentence1 = "(B) RAS activation enhances the binding of wild-type ASPP2 but not ASPP2 (S827A) to p53."
  val sentence2 = "Mechanistically ASPP1 and ASPP2 bind RAS-GTP and potentiates RAS signalling to enhance p53 mediated apoptosis [2]."
  val sentence3 = "Phosphorylation of ASPP2 by MAPK is required for RAS induced increased binding to p53 and increased transactivation of pro-apoptotic genes."
  val sentence4 = "We measured the rate of GAP mediated GTP hydrolysis and observed that the response of Ras ligated to UbiquitinC77 was identical to Ras ligated to UbiquitinG76C."

  sentence1 should "not contain a binding" in {
    val doc = bioproc annotate sentence1
    val mentions = extractor extractFrom doc
    assert(!mentions.exists(_.label == "Binding"))
  }

  sentence2 should "not include p53 in binding" in {
    val doc = bioproc annotate sentence2
    val mentions = extractor extractFrom doc
    val participants = Set("ASPP1", "ASPP2", "RAS-GTP")
    assert(mentions.exists(m => m.label == "Binding" && m.arguments("theme").map(_.text).toSet.diff(participants).isEmpty))
  }

  sentence3 should "have an up-regulated phosphorylation" in {
    val doc = bioproc annotate sentence3
    val mentions = extractor extractFrom doc
    assert(hasPositiveRegulationByEntity("MAPK", "Phosphorylation", Seq("ASPP2", "MAPK"), mentions))
  }

  sentence4 should "have two bindings with correct arguments" in {
    val doc = bioproc annotate sentence4
    val mentions = extractor extractFrom doc
    val participants1 = Set("Ras", "UbiquitinC77")
    val participants2 = Set("Ras", "UbiquitinG76C")
    assert(mentions.exists(m => m.label == "Binding" && m.arguments("theme").map(_.text).toSet.diff(participants1).isEmpty))
    assert(mentions.exists(m => m.label == "Binding" && m.arguments("theme").map(_.text).toSet.diff(participants2).isEmpty))
  }
}
