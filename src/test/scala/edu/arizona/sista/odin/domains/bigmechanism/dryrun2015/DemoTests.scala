package edu.arizona.sista.odin.domains.bigmechanism.dryrun2015

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import TestResources.{bioproc, extractor, summarizeError}
import DarpaEvalUtils._

/**
 * Unit tests related to the UI demo
 */
class DemoTests1 extends FunSuite with BeforeAndAfter {


  val text = "IKK contains two catalytic subunits, IKKalpha and IKKbeta, both of which are able to correctly phosphorylate IkappaB."
  val doc = bioproc.annotate(text)
  val mentions = extractor.extractFrom(doc)
  val assignedParty = "GUS"

  info(text)
  test("there should be a phosphorylation of IkappaB") {
    assert(hasEventWithArguments("Phosphorylation", List("IkappaB"), mentions), summarizeError(text, "Phosphorylation", assignedParty))
  }

  test("there should be an up-regulation between IKKalpha and the phosphorylation of IkappaB") {
    assert(hasPositiveRegulationByEntity("IKKalpha", "Phosphorylation", List("IkappaB"), mentions), summarizeError(text, "Positive_regulation", assignedParty))
  }

  test("there should be an up-regulation between IKKbeta and the phosphorylation of IkappaB") {
    assert(hasPositiveRegulationByEntity("IKKalpha", "Phosphorylation", List("IkappaB"), mentions), summarizeError(text, "Positive_regulation", assignedParty))
  }

  //test("In the future")(pending)
}


class DemoTests2 extends FunSuite with BeforeAndAfter {

  val text = "S6K1 phosphorylates the RPTOR protein and promotes the hydroxylation of the Pkh1 protein."
  val doc = bioproc.annotate(text)
  val mentions = extractor.extractFrom(doc)
  val assignedParty = "GUS"

  info(text)
  test("there should be a phosphorylation of RPTOR") {
    assert(hasEventWithArguments("Phosphorylation", List("RPTOR"), mentions), summarizeError(text, "Phosphorylation", assignedParty))
  }

  test("there should be a hydroxlyation of Pkh1") {
    assert(hasEventWithArguments("Hydroxylation", List("Pkh1"), mentions), summarizeError(text, "Hydroxylation", assignedParty))
  }

  test("there should be an up-regulation of the hydroxylation of Pkh1 by S6K1") {
    assert(hasPositiveRegulationByEntity("S6K1", "Hydroxylation", List("Pkh1"), mentions), summarizeError(text, "Positive_regulation", assignedParty))
  }

  //test("In the future")(pending)
}

class DemoTests3 extends FunSuite with BeforeAndAfter {

  val text = "Pkh1 phosphorylates the S6K1 protein. This phosphorylated protein binds with TopBP1."
  val doc = bioproc.annotate(text)
  val mentions = extractor.extractFrom(doc)
  val assignedParty = "GUS"

  info(text)
  test("there should be a phosphorylation of S6K1") {
    assert(hasEventWithArguments("Phosphorylation", List("S6K1"), mentions), summarizeError(text, "Phosphorylation", assignedParty))
  }

  test("there should be an up-regulation of the phosphorylation of S6K1 by Pkh1") {
    assert(hasPositiveRegulationByEntity("Pkh1", "Phosphorylation", List("S6K1"), mentions), summarizeError(text, "Positive_regulation", assignedParty))
  }

  //TODO Coref
  /**
   *
  test("there should be a binding between S6K1 and TopBP1") {
    assert(hasEventWithArguments("Binding", List("S6K1", "TopBP1"), mentions), summarizeError(text, "Binding", assignedParty))
  }
  */
  //test("In the future")(pending)
}
