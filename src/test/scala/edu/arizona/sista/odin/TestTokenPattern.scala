package edu.arizona.sista.odin

import org.scalatest._
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.odin.impl.{OdinCompileException, TokenPattern}

class TestTokenPattern extends FlatSpec with Matchers {
  val proc = new BioNLPProcessor
  val text1 = "TGFBR2 phosphorylates peri-kappa B and inhibits the ubiquitination of SMAD3."
  val text2 = "TGFBR2 phosphorylates peri-kappa B and peri-kappa C and inhibits the ubiquitination of SMAD3."
  val text3 = "TGFBR2 phosphorylates peri-kappa B and peri-kappa C by TGFBR3 and inhibits the ubiquitination of SMAD3."

  text1 should "contain one match" in {
    val doc = proc annotate text1
    val p = TokenPattern.compile("@Phosphorylation and inhibits")
    val mentions = Seq(
      new TextBoundMention("Phosphorylation", Interval(0, 4), 0, doc, true, "<MANUAL>")
    )
    val state = State(mentions)
    val results = p.findAllIn(0, doc, state)
    results should have size (1)
  }

  text2 should "contain one match" in {
    val doc = proc annotate text2
    val p = TokenPattern.compile("@Phosphorylation and inhibits")
    val mentions = Seq(
      new TextBoundMention("Phosphorylation", Interval(0, 4), 0, doc, true, "<MANUAL>"),
      new TextBoundMention("Phosphorylation", Interval(0, 7), 0, doc, true, "<MANUAL>")
    )
    val state = State(mentions)
    val results = p.findAllIn(0, doc, state)
    results should have size (1)
  }

  text3 should "contain two matches" in {
    val doc = proc annotate text3
    val p = TokenPattern.compile("@Phosphorylation and inhibits")
    val mentions = Seq(
      new TextBoundMention("Phosphorylation", Interval(0, 9), 0, doc, true, "<MANUAL>"),
      new TextBoundMention("Phosphorylation", Interval(0, 9), 0, doc, true, "<MANUAL>")
    )
    val state = State(mentions)
    val results = p.findAllIn(0, doc, state)
    results should have size (2)
  }

  val text4 = "a b c d e f g h i c"
  val doc = proc annotate text4

  text4 should "match with a lazy plus" in {
    val p = TokenPattern.compile("a []+? c")
    val results = p.findAllIn(0, doc)
    results should have size (1)
    results.head.interval should have (
      'start (0),
      'end (3)
    )
  }

  it should "match with a greedy plus" in {
    val p = TokenPattern.compile("a []+ c")
    val results = p.findAllIn(0, doc)
    results should have size (1)
    results.head.interval should have (
      'start (0),
      'end (10)
    )
  }

  it should "match with a lazy star" in {
    val p = TokenPattern.compile("a []*? c")
    val results = p.findAllIn(0, doc)
    results should have size (1)
    results.head.interval should have (
      'start (0),
      'end (3)
    )
  }

  it should "match with a greedy star" in {
    val p = TokenPattern.compile("a []* c")
    val results = p.findAllIn(0, doc)
    results should have size (1)
    results.head.interval should have (
      'start (0),
      'end (10)
    )
  }

  it should "match with positive lookbehind" in {
    val p = TokenPattern.compile("(?<=a) b")
    val results = p.findAllIn(0, doc)
    results should have size (1)
    results.head.interval should have (
      'start (1),
      'end (2)
    )
  }

  it should "match with several lookarounds" in {
    val p = TokenPattern.compile("(?<=a) b (?=c) | (?<=b) c (?=d)")
    val results = p.findAllIn(0, doc)
    results should have size (2)
    results(0).interval should have (
      'start (1),
      'end (2)
    )
    results(1).interval should have (
      'start (2),
      'end (3)
    )
  }

  it should "match with negative lookbehind that goes beyond sentence start" in {
    val p = TokenPattern.compile("(?<!x) a b c")
    val results = p.findAllIn(0, doc)
    results should have size (1)
  }

  it should "not match with positive lookbehind that goes beyond sentence start" in {
    val p = TokenPattern.compile("(?<=x) a b c")
    val results = p.findAllIn(0, doc)
    results should be ('empty)
  }

  it should "not match with negative lookbehind" in {
    val p = TokenPattern.compile("(?<!a) b")
    val results = p.findAllIn(0, doc)
    results should be ('empty)
  }

  it should "match with positive lookahead" in {
    val p = TokenPattern.compile("b (?=c)")
    val results = p.findAllIn(0, doc)
    results should have size (1)
    results.head.interval should have (
      'start (1),
      'end (2)
    )
  }

  it should "not match with negative lookahead" in {
    val p = TokenPattern.compile("b (?!c)")
    val results = p.findAllIn(0, doc)
    results should be ('empty)
  }

  it should "match nested captures" in {
    val rule = """
      |- name: test_rule
      |  priority: 1
      |  type: token
      |  label: TestCapture
      |  pattern: |
      |    a b (?<cap1>c d (?<cap2>e f) g h) i c
      |""".stripMargin
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc)

    results should have size (1)
    val rel = results.head
    rel.arguments should contain key ("cap1")
    rel.arguments("cap1") should have size (1)
    rel.arguments("cap1").head.text should be ("c d e f g h")
    rel.arguments should contain key ("cap2")
    rel.arguments("cap2") should have size (1)
    rel.arguments("cap2").head.text should be ("e f")
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
      |    @cause:BioChemicalEntity
      |    (?<trigger> [lemma="phosphorylate" & tag=/^V/])
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
      |    @cause:BioChemicalEntity
      |    (?<trigger>[lemma="phosphorylate" & tag=/^V/])
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
      |    @cause:BioChemicalEntity complex?
      |    (?<trigger>[lemma="phosphorylate" & tag=/^V/])
      |    [!tag=/^V/]*?
      |    @theme:BioChemicalEntity []+ @site:Site""".stripMargin

    val mentions = Seq(
      new TextBoundMention("BioChemicalEntity", Interval(0), 0, doc6, false, "<MANUAL>"),
      new TextBoundMention("BioChemicalEntity", Interval(0, 2), 0, doc6, false, "<MANUAL>"),
      new TextBoundMention("BioChemicalEntity", Interval(4), 0, doc6, false, "<MANUAL>"),
      new TextBoundMention("Site", Interval(7), 0, doc6, false, "<MANUAL>"),
      new TextBoundMention("Site", Interval(9), 0, doc6, false, "<MANUAL>"),
      new TextBoundMention("Site", Interval(11), 0, doc6, false, "<MANUAL>")
    )

    val state = State(mentions)
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc6, state)

    results should have size (2)

    val event1 = results(0)

    event1.arguments should contain key ("cause")
    event1.arguments("cause") should have size (1)
    event1.arguments("cause").head.text should be ("JAK3")

    event1.arguments should contain key ("theme")
    event1.arguments("theme") should have size (1)
    event1.arguments("theme").head.text should be ("HuR")

    event1.arguments should contain key ("site")
    event1.arguments("site") should have size (1)
    event1.arguments("site").head.text should be ("Y200")

    val event2 = results(1)

    event2.arguments should contain key ("cause")
    event2.arguments("cause") should have size (1)
    event2.arguments("cause").head.text should be ("JAK3 complex")

    event2.arguments should contain key ("theme")
    event2.arguments("theme") should have size (1)
    event2.arguments("theme").head.text should be ("HuR")

    event2.arguments should contain key ("site")
    event2.arguments("site") should have size (1)
    event2.arguments("site").head.text should be ("Y200")

  }

  val text7 = "JAK3 binds to MEK and RAS"
  val doc7 = proc annotate text7

  text7 should "match three mentions with argument name 'theme'" in {
    val rule = """
      |- name: test_rule
      |  priority: 1
      |  type: token
      |  label: Binding
      |  pattern: |
      |    @theme:Protein binds to @theme:Protein and @theme:Protein
      |""".stripMargin

    val mentions = Seq(
      new TextBoundMention("Protein", Interval(0), 0, doc7, false, "<MANUAL>"),
      new TextBoundMention("Protein", Interval(3), 0, doc7, false, "<MANUAL>"),
      new TextBoundMention("Protein", Interval(5), 0, doc7, false, "<MANUAL>")
    )

    val state = State(mentions)
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc7, state)

    results should have size (1)
    val binding = results.head
    binding.arguments should contain key ("theme")
    val themes = binding.arguments("theme")
    themes should have size (3)
    val themeTexts = themes.map(_.text)
    themeTexts should contain ("JAK3")
    themeTexts should contain ("RAS")
    themeTexts should contain ("MEK")

  }

  it should "capture three arguments with name 'theme'" in {
    val rule = """
      |- name: test_rule
      |  priority: 1
      |  type: token
      |  label: Binding
      |  pattern: |
      |    (?<theme>[]) binds to (?<theme>[]) and (?<theme>[])
      |""".stripMargin

    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc7)

    results should have size (1)
    val binding = results.head
    binding.arguments should contain key ("theme")
    val themes = binding.arguments("theme")
    themes should have size (3)
    val themeTexts = themes.map(_.text)
    themeTexts should contain ("JAK3")
    themeTexts should contain ("RAS")
    themeTexts should contain ("MEK")

  }

  it should "capture text and mentions with same argument name" in {
    val rule = """
      |- name: test_rule
      |  priority: 1
      |  type: token
      |  label: Binding
      |  pattern: |
      |    (?<theme>[]) binds to @theme:Protein and (?<theme>[])
      |""".stripMargin

    val mentions = Seq(
      new TextBoundMention("Protein", Interval(0), 0, doc7, false, "<MANUAL>"),
      new TextBoundMention("Protein", Interval(3), 0, doc7, false, "<MANUAL>"),
      new TextBoundMention("Protein", Interval(5), 0, doc7, false, "<MANUAL>")
    )

    val state = State(mentions)
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc7, state)

    results should have size (1)
    val binding = results.head
    binding.arguments should contain key ("theme")
    val themes = binding.arguments("theme")
    themes should have size (3)
    val themeTexts = themes.map(_.text)
    themeTexts should contain ("JAK3")
    themeTexts should contain ("RAS")
    themeTexts should contain ("MEK")

  }

  "rule" should "not compile with unit: \"lemma\"" in {
    val rule = """
                 |- name: test_rule
                 |  priority: 1
                 |  type: token
                 |  unit: lemma
                 |  label: Binding
                 |  pattern: |
                 |    (?<theme>[]) bind to @theme:Protein
                 |""".stripMargin

    an [OdinCompileException] should be thrownBy ExtractorEngine(rule)
  }

  it should "capture text and mentions using unit: \"tag\"" in {
    val rule = """
                 |- name: test_rule
                 |  priority: 1
                 |  type: token
                 |  unit: tag
                 |  label: Binding
                 |  pattern: |
                 |    (?<theme>[]) VBZ TO @theme:Protein
                 |""".stripMargin

    val mentions = Seq(
      new TextBoundMention("Protein", Interval(0), 0, doc7, false, "<MANUAL>"),
      new TextBoundMention("Protein", Interval(3), 0, doc7, false, "<MANUAL>")
    )

    val state = State(mentions)
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc7, state)

    results should have size (1)
    val binding = results.head
    binding.arguments should contain key ("theme")
    val themes = binding.arguments("theme")
    themes should have size (2)
    val themeTexts = themes.map(_.text)
    themeTexts should contain ("JAK3")
    themeTexts should contain ("MEK")

  }

  val text8 = "x a a b a b a b a b c d"
  val doc8 = proc annotate text8

  "TokenPattern" should "handle repetition in lookbehind" in {
    val p = TokenPattern.compile("(?<= a (a b){3} a) b ")
    val results = p.findAllIn(0, doc8)
    results should have size (1)
    results.head.interval should have (
      'start (9),
      'end (10)
    )
  }

}
