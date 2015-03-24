package edu.arizona.sista.odin.domains.bigmechanism.dryrun2015

import org.junit.Assert._
import org.junit.Test
import TestResources.{bioproc, extractor}
import DarpaEvalUtils._

/**
 * Testing for common syntactic variations on rules, e.g. passive voice, relative clauses, etc.
 */

class TestSyntacticVariants {

  /**
   * TODO: Coref
  @Test def testHydrolysisDecl1() {
    val doc = bioproc.annotate("RasGAP is hydrolyzing GTP to GDP in Ras reactions.")
    val mentions = extractor.extractFrom(doc)

    try {
      // TODO: fix hasEventWithArguments to match Complex (RelationMention) with desired argument.
      assertTrue("hydrolysis with COREF (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisSubjectDecl1")
        displayMentions(mentions, doc)
        throw e
    }
  }
*/
  @Test def testHydrolysisPass1() {
    val doc = bioproc.annotate("Ras-GDP is hydrolyzed by 26S proteasome without ubiquitination.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisSubjectPass1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydrolysisSubjNom1() {
    val doc = bioproc.annotate("MEK hydrolysis of Ras-GDP increased.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisSubjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydrolysisObjNom1() {
    val doc = bioproc.annotate("Ras-GDP hydrolysis by MEK increased.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisObjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydrolysisSubjectRel1() {
    val doc = bioproc.annotate("Its many abnormal phenotypes can be rescued via Pde2, which specifically hydrolyzes Ras-GDP.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisSubjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydrolysisSubjectRel2() {
    val doc = bioproc.annotate("Pde2, which has been found to hydrolyze Ras-GDP, activates MEK.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisSubjectRel2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydrolysisSubjectRelApposition1() {
    val doc = bioproc.annotate("Its many abnormal phenotypes can be rescued via overexpressing Pde2, a phosphodiesterase that specifically hydrolyzes Ras-GDP.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GDP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisSubjectRelApposition1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydrolysisSubjectRelApposition2() {
    val doc = bioproc.annotate("A main rate-controlling step in RAS is renin, an enzyme that hydrolyzes Ras-GTP.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("Ras-GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisSubjectApposition2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydrolysisObjectRel1() {
    val doc = bioproc.annotate("We measured transcription activation in the presence of MEK, which is hydrolyzed by CRP.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("hydrolysis (DANE)", hasEventWithArguments("Hydrolysis", List("MEK"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydrolysisObjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingDecl1() {
    val doc = bioproc.annotate("Mechanistically, ASPP1 and ASPP2 bind RAS-GTP.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("ASPP1", "ASPP2", "RAS-GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingDecl1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingDecl2() {
    val doc = bioproc.annotate("Mechanistically, ASPP1 and ASPP2 bind with RAS-GTP.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("ASPP1", "ASPP2", "RAS-GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingDecl2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingPass1() {
    val doc = bioproc.annotate("Mechanistically, ASPP1 and ASPP2 are bound by RAS-GTP.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("ASPP1", "ASPP2", "RAS-GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingPass1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingPrepNom1() {
    val doc = bioproc.annotate("We detected elevated binding of p53 to K-Ras.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingPrepNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingPrepNom2() {
    val doc = bioproc.annotate("We detected elevated binding of p53 and K-Ras.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingPrepNom2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingPrepNom3() {
    val doc = bioproc.annotate("We detected elevated binding of p53 with K-Ras.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingPrepNom3")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingSubjNom1() {
    val doc = bioproc.annotate("We detected elevated p53 binding to K-Ras.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingSubjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingObjNom1() {
    val doc = bioproc.annotate("We detected elevated K-Ras binding by p53.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingObjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingSubjRel1() {
    val doc = bioproc.annotate("We detected elevated phosphorylation of K-Ras, a protein that subsequently binds p53.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingSubjRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testBindingObjRel1() {
    val doc = bioproc.annotate("We detected elevated phosphorylation of K-Ras, a protein that is subsequently bound by p53.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("binding (MARCO/GUS)", hasEventWithArguments("Binding", List("p53", "K-Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header("testBindingObjRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testTransport1() {
    val doc = bioproc.annotate("Phosphorylation leads the plasma membrane to release p53 to the cytosol.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("transport (ENRIQUE)", hasEventWithArguments("Transport", List("p53", "plasma membrane", "cytosol"), mentions))
    } catch {
      case e: AssertionError =>
        header("testTransport1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testTransport2() {
    val doc = bioproc.annotate("Recruitment of p53 from the cytosol to the plasma membrane increases with phosphorylation.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("transport (ENRIQUE)", hasEventWithArguments("Transport", List("p53", "plasma membrane", "cytosol"), mentions))
    } catch {
      case e: AssertionError =>
        header("testTransport2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testTransport3() {
    val doc = bioproc.annotate("With increased phosphorylation, p53 is exported from the plasma membrane to the cytosol.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("transport (ENRIQUE)", hasEventWithArguments("Transport", List("p53", "plasma membrane", "cytosol"), mentions))
    } catch {
      case e: AssertionError =>
        header("testTransport3")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testTransport4() {
    val doc = bioproc.annotate("ASPP2, a protein which is transported from the membrane to the nucleus, is subsequently phosphorylated.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("transport (ENRIQUE)", hasEventWithArguments("Transport", List("ASPP2", "membrane", "nucleus"), mentions))
    } catch {
      case e: AssertionError =>
        header("testTransport4")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testTransport5() {
    val doc = bioproc.annotate("ASPP2, a protein which translocates Pde2 from the membrane to the nucleus, is subsequently phosphorylated.")
    val mentions = extractor.extractFrom(doc)

    try {
      assertTrue("transport (ENRIQUE)", hasEventWithArguments("Transport", List("Pde2", "membrane", "nucleus"), mentions))
    } catch {
      case e: AssertionError =>
        header("testTransport4")
        displayMentions(mentions, doc)
        throw e
    }
  }

  // Phospho tests
  @Test def testPhosphorylationDecl1() {
    val doc = bioproc.annotate("Ras is phosphorylating ASPP2.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectDecl1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationPass1() {
    val doc = bioproc.annotate("ASPP2 is phosphorylated by Ras.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectPass1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationSubjNom1() {
    val doc = bioproc.annotate("Ras phosphorylation of ASPP2 increased.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationObjNom1() {
    val doc = bioproc.annotate("ASPP2 phosphorylation by Ras increased.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header("testPhosphorylationObjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationSubjectRel1() {
    val doc = bioproc.annotate("Its many abnormal phenotypes can be rescued via Ras, which specifically phosphorylates ASPP2.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationSubjectRel2() {
    val doc = bioproc.annotate("Ras, which has been found to phosphorylate ASPP2, activates MEK.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRel2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationSubjectRelApposition1() {
    val doc = bioproc.annotate("Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that specifically phosphorylates ASPP2.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRelApposition1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationSubjectRelApposition2() {
    val doc = bioproc.annotate("A main rate-controlling step in AAAA is renin, an enzyme that phosphorylates ASPP2 to generate XXXX")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectApposition2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testPhosphorylationObjectRel1() {
    val doc = bioproc.annotate("We measured transcription activation in the presence of ASPP2, which is phosphorylated by Ras.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Phosphorylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}ObjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  // Hydrox tests
  @Test def testHydroxylationDecl1() {
    val doc = bioproc.annotate("Ras is hydroxylating ASPP2.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectDecl1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationPass1() {
    val doc = bioproc.annotate("ASPP2 is hydroxylated by Ras.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectPass1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationSubjNom1() {
    val doc = bioproc.annotate("Ras hydroxylation of ASPP2 increased.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationObjNom1() {
    val doc = bioproc.annotate("ASPP2 hydroxylation by Ras increased.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header("testHydroxylationObjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationSubjectRel1() {
    val doc = bioproc.annotate("Its many abnormal phenotypes can be rescued via Ras, which specifically hydroxylates ASPP2.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationSubjectRel2() {
    val doc = bioproc.annotate("Ras, which has been found to hydroxylate ASPP2, activates MEK.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRel2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationSubjectRelApposition1() {
    val doc = bioproc.annotate("Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that specifically hydroxylates ASPP2.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRelApposition1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationSubjectRelApposition2() {
    val doc = bioproc.annotate("A main rate-controlling step in AAAA is renin, an enzyme that hydroxylates ASPP2 to generate XXXX")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectApposition2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testHydroxylationObjectRel1() {
    val doc = bioproc.annotate("We measured transcription activation in the presence of ASPP2, which is hydroxylated by Ras.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Hydroxylation"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}ObjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }


  // Ubiq tests
  @Test def testUbiquitinationDecl1() {
    val doc = bioproc.annotate("Ras is ubiquitinating ASPP2.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectDecl1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationPass1() {
    val doc = bioproc.annotate("ASPP2 is ubiquitinated by Ras.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectPass1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationSubjNom1() {
    val doc = bioproc.annotate("Ras ubiquitination of ASPP2 increased.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationObjNom1() {
    val doc = bioproc.annotate("ASPP2 ubiquitination by Ras increased.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header("testUbiquitinationObjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationObjNom2() {
    val doc = bioproc.annotate("RAS ubiquitination and degradation by ASPP2 and p53 increased.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("RAS"), mentions))
      assertTrue(s"ASPP2 regulation ($assignedParty)", hasPositiveRegulationByEntity("ASPP2", eventLabel, List("RAS"), mentions))
      assertTrue(s"p53 regulation ($assignedParty)", hasPositiveRegulationByEntity("p53", eventLabel, List("RAS"), mentions))
    } catch {
      case e: AssertionError =>
        header("testUbiquitinationObjNom2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationSubjectRel1() {
    val doc = bioproc.annotate("Its many abnormal phenotypes can be rescued via Ras, which specifically ubiquitinates ASPP2.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationSubjectRel2() {
    val doc = bioproc.annotate("Ras, which has been found to ubiquitinate ASPP2, activates MEK.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRel2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationSubjectRelApposition1() {
    val doc = bioproc.annotate("Its many abnormal phenotypes can be rescued via overexpressing Ras, an XXX that specifically ubiquitinates ASPP2.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectRelApposition1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationSubjectRelApposition2() {
    val doc = bioproc.annotate("A main rate-controlling step in AAAA is renin, an enzyme that ubiquitinates ASPP2 to generate XXXX")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjectApposition2")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testUbiquitinationObjectRel1() {
    val doc = bioproc.annotate("We measured transcription activation in the presence of ASPP2, which is ubiquitinated by Ras.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Ubiquitination"
    val assignedParty = "GUS"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("ASPP2"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("Ras", eventLabel, List("ASPP2"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}ObjectRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testDegradationDecl1() {
    val doc = bioproc.annotate("ASPP2 degraded KRAS and RAS.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Degradation"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("KRAS","RAS"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("ASPP2", eventLabel, List("KRAS", "RAS"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}Decl1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testDegradationPass1() {
    val doc = bioproc.annotate("KRAS and RAS are both degraded by ASPP2.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Degradation"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("KRAS","RAS"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("ASPP2", eventLabel, List("KRAS", "RAS"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}Pass1")
        displayMentions(mentions, doc)
        throw e
    }
  }


  @Test def testDegradationPrepNom1() {
    val doc = bioproc.annotate("The ubiquitination and degradation of RAS by ASPP2 increased.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Degradation"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("RAS"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("ASPP2", eventLabel, List("RAS"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}PrepNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }


  @Test def testDegradationObjNom1() {
    val doc = bioproc.annotate("RAS ubiquitination and degradation by ASPP2 increased.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Degradation"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("RAS"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("ASPP2", eventLabel, List("RAS"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}ObjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }


  @Test def testDegradationSubjNom1() {
    val doc = bioproc.annotate("ASPP2 ubiquitination and degradation of Ras increased.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Degradation"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("Ras"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("ASPP2", eventLabel, List("Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }


  @Test def testDegradationSubjRel1() {
    val doc = bioproc.annotate("Its many abnormal phenotypes can be rescued via Pde2, which specifically degrades Ras.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Degradation"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("Ras"), mentions))
      assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("Pde2", eventLabel, List("Ras"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }

  @Test def testExchangeDecl1() {
    val doc = bioproc.annotate("Ras exchanges GDP for GTP more rapidly in the presence of Pde2.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Exchange"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("Ras", "GDP", "GTP"), mentions))
      // TODO: amend to find Pde2 (DANE)
      // assertTrue(s"up-regulation ($assignedParty)", hasPositiveRegulationByEntity("Pde2", eventLabel, List("Ras", "GDP", "GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}Decl1")
        displayMentions(mentions, doc)
        throw e
    }
  }


  @Test def testExchangePass1() {
    val doc = bioproc.annotate("the GDP bound to the Ras protein is not exchanged for GTP.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Exchange"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("Ras", "GDP", "GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}Pass1")
        displayMentions(mentions, doc)
        throw e
    }
  }

/**
 * TODO: Coref
 @Test def testExchangePrepNom1() {
    val doc = bioproc.annotate("In RAS, the exchange of GDP for GTP increased.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Exchange"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} with COREF ($assignedParty)", hasEventWithArguments(eventLabel, List("Ras", "GDP", "GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}PrepNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }
*/

/**
 * TODO: Coref
  @Test def testExchangeObjNom1() {
    val doc = bioproc.annotate("In Ras, GDP exchange for GTP increased.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Exchange with COREF"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("Ras", "GDP", "GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}ObjNom1")
        displayMentions(mentions, doc)
        throw e
    }
  }
*/

  @Test def testExchangeSubjRel1() {
    val doc = bioproc.annotate("Its many abnormal phenotypes can be rescued via Pde2, which normally exchanges GDP with GTP.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Exchange"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("GDP", "GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}SubjRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }


  @Test def testExchangeObjRel1() {
    val doc = bioproc.annotate("Its many abnormal phenotypes can be rescued via GDP, which is normally exchanged with GTP in Ras.")
    val mentions = extractor.extractFrom(doc)
    val eventLabel = "Exchange"
    val assignedParty = "DANE"

    try {
      assertTrue(s"${eventLabel.toLowerCase} ($assignedParty)", hasEventWithArguments(eventLabel, List("Ras", "GDP", "GTP"), mentions))
    } catch {
      case e: AssertionError =>
        header(s"test${eventLabel}ObjRel1")
        displayMentions(mentions, doc)
        throw e
    }
  }
}
