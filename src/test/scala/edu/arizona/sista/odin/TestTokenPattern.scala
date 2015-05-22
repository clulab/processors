package edu.arizona.sista.odin

import org.scalatest._
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.odin.impl.TokenPattern
import edu.arizona.sista.odin._

class TestTokenPattern extends FlatSpec with Matchers {
  val proc = new BioNLPProcessor
  val text1 = "TGFBR2 phosphorylates peri-kappa B and inhibits the ubiquitination of SMAD3."
  val text2 = "TGFBR2 phosphorylates peri-kappa B and peri-kappa C and inhibits the ubiquitination of SMAD3."
  val text3 = "TGFBR2 phosphorylates peri-kappa B and peri-kappa C by TGFBR3 and inhibits the ubiquitination of SMAD3."

  text1 should "contain one match" in {
    val doc = proc annotate text1
    val p = TokenPattern.compile("@Phosphorylation and inhibits")
    val mentions = Seq(new TextBoundMention("Phosphorylation", Interval(0, 4), 0, doc, true, "<MANUAL>"))
    val state = State(mentions)
    val results = p.findAllIn(0, doc, state)
    results should have size (1)
  }

  text2 should "contain one match" in {
    val doc = proc annotate text2
    val p = TokenPattern.compile("@Phosphorylation and inhibits")
    val mentions = Seq(new TextBoundMention("Phosphorylation", Interval(0, 4), 0, doc, true, "<MANUAL>"),
                       new TextBoundMention("Phosphorylation", Interval(0, 7), 0, doc, true, "<MANUAL>"))
    val state = State(mentions)
    val results = p.findAllIn(0, doc, state)
    results should have size (1)
  }

  text3 should "contain two matches" in {
    val doc = proc annotate text3
    val p = TokenPattern.compile("@Phosphorylation and inhibits")
    val mentions = Seq(new TextBoundMention("Phosphorylation", Interval(0, 9), 0, doc, true, "<MANUAL>"),
                       new TextBoundMention("Phosphorylation", Interval(0, 9), 0, doc, true, "<MANUAL>"))
    val state = State(mentions)
    val results = p.findAllIn(0, doc, state)
    results should have size (2)
  }

  val text4 = "a b c d e f g h i c"
  val doc = proc annotate text4

  text4 should "match with a lazy plus" in {
    val p = TokenPattern.compile("a []+? c")
    val results = p.findAllIn(0, doc, None)
    results should have size (1)
    results.head.interval should have (
      'start (0),
      'end (3)
    )
  }

  it should "match with a greedy plus" in {
    val p = TokenPattern.compile("a []+ c")
    val results = p.findAllIn(0, doc, None)
    results should have size (1)
    results.head.interval should have (
      'start (0),
      'end (10)
    )
  }

  it should "match with a lazy star" in {
    val p = TokenPattern.compile("a []*? c")
    val results = p.findAllIn(0, doc, None)
    results should have size (1)
    results.head.interval should have (
      'start (0),
      'end (3)
    )
  }

  it should "match with a greedy star" in {
    val p = TokenPattern.compile("a []* c")
    val results = p.findAllIn(0, doc, None)
    results should have size (1)
    results.head.interval should have (
      'start (0),
      'end (10)
    )
  }

  val text5 = "JAK3 phosphorylates three HuR residues (Y63, Y68, Y200)"
  val doc5 = proc annotate text5

  text5 should "match Y200 using greedy plus" in {
    val p = TokenPattern.compile("[]+ @site:Site")
    val mentions = Seq(
      new TextBoundMention("Site", Interval(6), 0, doc5, true, "<MANUAL>"),
      new TextBoundMention("Site", Interval(8), 0, doc5, true, "<MANUAL>"),
      new TextBoundMention("Site", Interval(10), 0, doc5, true, "<MANUAL>")
    )
    val state = State(mentions)
    val results = p.findAllIn(0, doc5, state)
    results should have size (1)
    results.head.interval should have (
      'start (0),
      'end (11)
    )
  }

  it should "match Y63 using lazy plus" in {
    val p = TokenPattern.compile("[]+? @site:Site")
    val mentions = Seq(
      new TextBoundMention("Site", Interval(6), 0, doc5, true, "<MANUAL>"),
      new TextBoundMention("Site", Interval(8), 0, doc5, true, "<MANUAL>"),
      new TextBoundMention("Site", Interval(10), 0, doc5, true, "<MANUAL>")
    )
    val state = State(mentions)
    val results = p.findAllIn(0, doc5, state)
    results should have size (3)
    results(0).interval should have (
      'start (0),
      'end (7)
    )
    results(1).interval should have (
      'start (7),
      'end (9)
    )
    results(2).interval should have (
      'start (9),
      'end (11)
    )

  }

  it should "match event with lazy plus" in {
    val rule = """
      |- name: test_rule
      |  priority: 1
      |  type: token
      |  label: Phosphorylation
      |  pattern: |
      |    (@cause:BioChemicalEntity)?
      |    (?<trigger> [lemma="phosphorylate" & tag=/^V/ & !mention=ModificationTrigger])
      |    [!tag=/^V/]*?
      |    @theme:BioChemicalEntity []+? @site:Site""".stripMargin

    val mentions = Seq(
      new TextBoundMention("BioChemicalEntity", Interval(0), 0, doc5, false, "<MANUAL>"),
      new TextBoundMention("BioChemicalEntity", Interval(3), 0, doc5, false, "<MANUAL>"),
      new TextBoundMention("Site", Interval(6), 0, doc5, false, "<MANUAL>"),
      new TextBoundMention("Site", Interval(8), 0, doc5, false, "<MANUAL>"),
      new TextBoundMention("Site", Interval(10), 0, doc5, false, "<MANUAL>")
    )

    val state = State(mentions)
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc5, state)

    results should have size (1)
    val event = results.head

    event.arguments should contain key ("cause")
    event.arguments("cause") should have size (1)
    event.arguments("cause").head.text should be ("JAK3")

    event.arguments should contain key ("theme")
    event.arguments("theme") should have size (1)
    event.arguments("theme").head.text should be ("HuR")

    event.arguments should contain key ("site")
    event.arguments("site") should have size (1)
    event.arguments("site").head.text should be ("Y63")
  }

  it should "match event with greedy plus" in {
    val rule = """
      |- name: test_rule
      |  priority: 1
      |  type: token
      |  label: Phosphorylation
      |  pattern: |
      |    (@cause:BioChemicalEntity)?
      |    (?<trigger> [lemma="phosphorylate" & tag=/^V/ & !mention=ModificationTrigger])
      |    [!tag=/^V/]*?
      |    @theme:BioChemicalEntity []+ @site:Site""".stripMargin

    val mentions = Seq(
      new TextBoundMention("BioChemicalEntity", Interval(0), 0, doc5, false, "<MANUAL>"),
      new TextBoundMention("BioChemicalEntity", Interval(3), 0, doc5, false, "<MANUAL>"),
      new TextBoundMention("Site", Interval(6), 0, doc5, false, "<MANUAL>"),
      new TextBoundMention("Site", Interval(8), 0, doc5, false, "<MANUAL>"),
      new TextBoundMention("Site", Interval(10), 0, doc5, false, "<MANUAL>")
    )

    val state = State(mentions)
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc5, state)

    results should have size (1)
    val event = results.head

    event.arguments should contain key ("cause")
    event.arguments("cause") should have size (1)
    event.arguments("cause").head.text should be ("JAK3")

    event.arguments should contain key ("theme")
    event.arguments("theme") should have size (1)
    event.arguments("theme").head.text should be ("HuR")

    event.arguments should contain key ("site")
    event.arguments("site") should have size (1)
    event.arguments("site").head.text should be ("Y200")
  }

  val text6 = "JAK3 complex phosphorylates three HuR residues (Y63, Y68, Y200)"
  val doc6 = proc annotate text6

  text6 should "match event when there are several entity mentions" in {
    val rule = """
      |- name: test_rule
      |  priority: 1
      |  type: token
      |  label: Phosphorylation
      |  pattern: |
      |    (@cause:BioChemicalEntity)? complex?
      |    (?<trigger> [lemma="phosphorylate" & tag=/^V/ & !mention=ModificationTrigger])
      |    [!tag=/^V/]*?
      |    @theme:BioChemicalEntity []+ @site:Site""".stripMargin

    val mentions = Seq(
      new TextBoundMention("BioChemicalEntity", Interval(0), 0, doc6, false, "<MANUAL>"),
      new TextBoundMention("BioChemicalEntity", Interval(0, 2), 0, doc6, false, "<MANUAL>"),
      new TextBoundMention("BioChemicalEntity", Interval(3), 0, doc6, false, "<MANUAL>"),
      new TextBoundMention("Site", Interval(6), 0, doc6, false, "<MANUAL>"),
      new TextBoundMention("Site", Interval(8), 0, doc6, false, "<MANUAL>"),
      new TextBoundMention("Site", Interval(10), 0, doc6, false, "<MANUAL>")
    )

    val state = State(mentions)
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc6, state)

    results should have size (2)

    val event1 = results(0)

    event1.arguments should contain key ("cause")
    event1.arguments("cause") should have size (1)
    event1.arguments("cause").head.text should contain ("JAK3")

    event1.arguments should contain key ("theme")
    event1.arguments("theme") should have size (1)
    event1.arguments("theme").head.text should be ("HuR")

    event1.arguments should contain key ("site")
    event1.arguments("site") should have size (1)
    event1.arguments("site").head.text should be ("Y200")

    val event2 = results(0)

    event2.arguments should contain key ("cause")
    event2.arguments("cause") should have size (1)
    event2.arguments("cause").head.text should contain ("JAK3")

    event2.arguments should contain key ("theme")
    event2.arguments("theme") should have size (1)
    event2.arguments("theme").head.text should be ("HuR")

    event2.arguments should contain key ("site")
    event2.arguments("site") should have size (1)
    event2.arguments("site").head.text should be ("Y200")

  }

}
