package org.clulab.odin

import org.clulab.odin.impl.{OdinCompileException, TokenPattern}
import org.clulab.struct.Interval
import org.scalatest._
import org.clulab.TestUtils.jsonStringToDocument
import org.clulab.processors.{Document, Sentence}

class TestTokenPattern extends FlatSpec with Matchers {

  val text1 = "TGFBR2 phosphorylates peri-kappa B and inhibits the ubiquitination of SMAD3."
  val text2 = "TGFBR2 phosphorylates peri-kappa B and peri-kappa C and inhibits the ubiquitination of SMAD3."
  val text3 = "TGFBR2 phosphorylates peri-kappa B and peri-kappa C by TGFBR3 and inhibits the ubiquitination of SMAD3."

  text1 should "contain one match" in {
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["TGFBR2","phosphorylates","peri-kappa","B","and","inhibits","the","ubiquitination","of","SMAD3","."],"startOffsets":[0,7,22,33,35,39,48,52,67,70,75],"endOffsets":[6,21,32,34,38,47,51,66,69,75,76],"tags":["NN","VBZ","NN","NN","CC","VBZ","DT","NN","IN","NN","."],"lemmas":["tgfbr2","phosphorylate","peri-kappa","b","and","inhibit","the","ubiquitination","of","smad3","."],"entities":["B-Gene_or_gene_product","O","O","O","O","O","O","O","O","B-Gene_or_gene_product","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","O","B-VP","B-NP","I-NP","B-PP","B-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":3,"relation":"dobj"},{"source":1,"destination":4,"relation":"cc"},{"source":1,"destination":5,"relation":"conj"},{"source":3,"destination":2,"relation":"nn"},{"source":5,"destination":7,"relation":"dobj"},{"source":7,"destination":6,"relation":"det"},{"source":7,"destination":8,"relation":"prep"},{"source":8,"destination":9,"relation":"pobj"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":3,"relation":"dobj"},{"source":1,"destination":5,"relation":"conj_and"},{"source":3,"destination":2,"relation":"nn"},{"source":5,"destination":0,"relation":"nsubj"},{"source":5,"destination":7,"relation":"dobj"},{"source":7,"destination":6,"relation":"det"},{"source":7,"destination":9,"relation":"prep_of"}],"roots":[1]}}}]} """)
    val p = TokenPattern.compile("@Phosphorylation and inhibits")
    val mentions = Seq(
      new TextBoundMention("Phosphorylation", Interval(0, 4), 0, doc, true, "<MANUAL>")
    )
    val state = State(mentions)
    val results = p.findAllIn(0, doc, state)
    results should have size (1)
  }

  text2 should "contain one match" in {
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["TGFBR2","phosphorylates","peri-kappa","B","and","peri-kappa","C","and","inhibits","the","ubiquitination","of","SMAD3","."],"startOffsets":[0,7,22,33,35,39,50,52,56,65,69,84,87,92],"endOffsets":[6,21,32,34,38,49,51,55,64,68,83,86,92,93],"tags":["NN","VBZ","NN","NN","CC","NN","NN","CC","VBZ","DT","NN","IN","NN","."],"lemmas":["tgfbr2","phosphorylate","peri-kappa","b","and","peri-kappa","c","and","inhibit","the","ubiquitination","of","smad3","."],"entities":["B-Gene_or_gene_product","O","O","O","O","O","O","O","O","O","O","O","B-Gene_or_gene_product","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","O","B-NP","I-NP","O","B-VP","B-NP","I-NP","B-PP","B-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":3,"relation":"dobj"},{"source":1,"destination":7,"relation":"cc"},{"source":1,"destination":8,"relation":"conj"},{"source":3,"destination":2,"relation":"nn"},{"source":3,"destination":4,"relation":"cc"},{"source":3,"destination":6,"relation":"conj"},{"source":6,"destination":5,"relation":"nn"},{"source":8,"destination":10,"relation":"dobj"},{"source":10,"destination":9,"relation":"det"},{"source":10,"destination":11,"relation":"prep"},{"source":11,"destination":12,"relation":"pobj"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":3,"relation":"dobj"},{"source":1,"destination":6,"relation":"dobj"},{"source":1,"destination":8,"relation":"conj_and"},{"source":3,"destination":2,"relation":"nn"},{"source":3,"destination":6,"relation":"conj_and"},{"source":6,"destination":5,"relation":"nn"},{"source":8,"destination":0,"relation":"nsubj"},{"source":8,"destination":10,"relation":"dobj"},{"source":10,"destination":9,"relation":"det"},{"source":10,"destination":12,"relation":"prep_of"}],"roots":[1]}}}]} """)
    val p = TokenPattern.compile("@Phosphorylation and inhibits")
    val mentions = Seq(
      new TextBoundMention("Phosphorylation", Interval(0, 4), 0, doc, true, "<MANUAL>"),
      new TextBoundMention("Phosphorylation", Interval(0, 7), 0, doc, true, "<MANUAL>")
    )
    val state = State(mentions)
    val results = p.findAllIn(0, doc, state)
    results should have size (1)
  }

  // test overlaps
  val doc3 = jsonStringToDocument(""" {"sentences":[{"words":["TGFBR2","phosphorylates","peri-kappa","B","and","peri-kappa","C","by","TGFBR3","and","inhibits","the","ubiquitination","of","SMAD3","."],"startOffsets":[0,7,22,33,35,39,50,52,55,62,66,75,79,94,97,102],"endOffsets":[6,21,32,34,38,49,51,54,61,65,74,78,93,96,102,103],"tags":["NN","VBZ","NN","NN","CC","NN","NN","IN","NN","CC","VBZ","DT","NN","IN","NN","."],"lemmas":["tgfbr2","phosphorylate","peri-kappa","b","and","peri-kappa","c","by","tgfbr3","and","inhibit","the","ubiquitination","of","smad3","."],"entities":["B-Gene_or_gene_product","O","O","O","O","O","O","O","B-Gene_or_gene_product","O","O","O","O","O","B-Gene_or_gene_product","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","O","B-NP","I-NP","B-PP","B-NP","O","B-VP","B-NP","I-NP","B-PP","B-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":3,"relation":"dobj"},{"source":1,"destination":9,"relation":"cc"},{"source":1,"destination":10,"relation":"conj"},{"source":3,"destination":2,"relation":"nn"},{"source":3,"destination":4,"relation":"cc"},{"source":3,"destination":6,"relation":"conj"},{"source":3,"destination":7,"relation":"prep"},{"source":6,"destination":5,"relation":"nn"},{"source":7,"destination":8,"relation":"pobj"},{"source":10,"destination":12,"relation":"dobj"},{"source":12,"destination":11,"relation":"det"},{"source":12,"destination":13,"relation":"prep"},{"source":13,"destination":14,"relation":"pobj"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":3,"relation":"dobj"},{"source":1,"destination":6,"relation":"dobj"},{"source":1,"destination":10,"relation":"conj_and"},{"source":3,"destination":2,"relation":"nn"},{"source":3,"destination":6,"relation":"conj_and"},{"source":3,"destination":8,"relation":"prep_by"},{"source":6,"destination":5,"relation":"nn"},{"source":10,"destination":0,"relation":"nsubj"},{"source":10,"destination":12,"relation":"dobj"},{"source":12,"destination":11,"relation":"det"},{"source":12,"destination":14,"relation":"prep_of"}],"roots":[1]}}}]} """)
  text3 should "contain two matches" in {
    val p = TokenPattern.compile("@ph:Phosphorylation (by TGFBR3)? and inhibits")
    val mentions = Seq(
      new TextBoundMention("Phosphorylation", Interval(0, 7), 0, doc3, true, "<MANUAL>"),
      new TextBoundMention("Phosphorylation", Interval(0, 9), 0, doc3, true, "<MANUAL>")
    )
    val state = State(mentions)
    val results = p.findAllIn(0, doc3, state).distinct
    results should have size (2)
  }

  // test overlaps
  it should "also contain two matches if mentions in state are reversed" in {
    val p = TokenPattern.compile("@ph:Phosphorylation (by TGFBR3)? and inhibits")
    val mentions = Seq(
      new TextBoundMention("Phosphorylation", Interval(0, 9), 0, doc3, true, "<MANUAL>"),
      new TextBoundMention("Phosphorylation", Interval(0, 7), 0, doc3, true, "<MANUAL>")
    )
    val state = State(mentions)
    val results = p.findAllIn(0, doc3, state).distinct
    results should have size (2)
  }

  val text4 = "a b c d e f g h i c"
  val doc = Document(
    Array(
      Sentence(
        text4.split(" "),
        Array(0, 2, 4, 6, 8, 10, 12, 14, 16, 18),
        Array(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
      )
    )
  )

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

  // a b c d e f g h i c

  it should "keep complete match interval when capturing a RelationMention" in {
    val rule = """
      |- name: testrule
      |  priority: 1
      |  type: token
      |  label: TestCapture
      |  pattern: |
      |    b c (?<args>d e) f (?<args>g h) i
      |""".stripMargin
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc)
    results should have size (1)
    val r = results.head
    r shouldBe a [RelationMention]
    r.arguments should contain key ("args")
    r.arguments("args") should have size (2)
    val Seq(a1, a2) = r.arguments("args").sorted
    a1.tokenInterval should have (
      'start (3),
      'end (5)
    )
    a2.tokenInterval should have (
      'start (6),
      'end (8)
    )
    r.tokenInterval should have (
      'start (1),
      'end (9)
    )
  }

  it should "keep complete match interval when capturing an EventMention" in {
    val rule = """
      |- name: testrule
      |  priority: 1
      |  type: token
      |  label: TestCapture
      |  pattern: |
      |    b c (?<args>d e) (?<trigger> f) (?<args>g h) i
      |""".stripMargin
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc)
    results should have size (1)
    val r = results.head
    r shouldBe an [EventMention]
    r.arguments should contain key ("args")
    r.arguments("args") should have size (2)
    val Seq(a1, a2) = r.arguments("args").sorted
    r.asInstanceOf[EventMention].trigger.tokenInterval should have (
      'start (5),
      'end (6)
    )
    a1.tokenInterval should have (
      'start (3),
      'end (5)
    )
    a2.tokenInterval should have (
      'start (6),
      'end (8)
    )
    r.tokenInterval should have (
      'start (1),
      'end (9)
    )
  }

  it should "match with variable length lookbehind" in {
    val p = TokenPattern.compile("(?<=a []+) e")
    val results = p.findAllIn(0, doc)
    results should have size (1)
    results.head.interval should have (
      'start (4),
      'end (5)
    )
  }

  it should "match with nested lookbehind" in {
    val p = TokenPattern.compile("(?<= (?<= (?<= a b) c d) e) f")
    val results = p.findAllIn(0, doc)
    results should have size (1)
    results.head.interval should have (
      'start (5),
      'end (6)
    )
  }

  it should "match with lookahead in lookbehind" in {
    val p = TokenPattern.compile("(?<= a b (?=c d)) c")
    val results = p.findAllIn(0, doc)
    results should have size (1)
    results.head.interval should have (
      'start (2),
      'end (3)
    )
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
  val doc5 = jsonStringToDocument(""" {"sentences":[{"words":["JAK3","phosphorylates","three","HuR","residues","(","Y63",",","Y68",",","Y200",")"],"startOffsets":[0,5,20,26,30,39,40,43,45,48,50,54],"endOffsets":[4,19,25,29,38,40,43,44,48,49,54,55],"tags":["NN","VBZ","CD","NN","NNS","-LRB-","NN",",","NN",",","NN","-RRB-"],"lemmas":["jak3","phosphorylate","three","hur","residue","(","y63",",","y68",",","y200",")"],"entities":["B-Gene_or_gene_product","O","O","B-Gene_or_gene_product","O","O","O","O","O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","I-NP","I-NP","I-NP","O","B-NP","O","B-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":4,"relation":"dobj"},{"source":4,"destination":2,"relation":"num"},{"source":4,"destination":3,"relation":"nn"},{"source":4,"destination":10,"relation":"appos"},{"source":10,"destination":6,"relation":"nn"},{"source":10,"destination":8,"relation":"dep"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":4,"relation":"dobj"},{"source":4,"destination":2,"relation":"num"},{"source":4,"destination":3,"relation":"nn"},{"source":4,"destination":10,"relation":"appos"},{"source":10,"destination":6,"relation":"nn"},{"source":10,"destination":8,"relation":"dep"}],"roots":[1]}}}]} """)

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
  val doc6 = jsonStringToDocument(""" {"sentences":[{"words":["JAK3","complex","phosphorylates","three","HuR","residues","(","Y63",",","Y68",",","Y200",")"],"startOffsets":[0,5,13,28,34,38,47,48,51,53,56,58,62],"endOffsets":[4,12,27,33,37,46,48,51,52,56,57,62,63],"tags":["NN","NN","VBZ","CD","NN","NNS","-LRB-","NN",",","NN",",","NN","-RRB-"],"lemmas":["jak3","complex","phosphorylate","three","hur","residue","(","y63",",","y68",",","y200",")"],"entities":["B-Gene_or_gene_product","O","O","O","B-Gene_or_gene_product","O","O","O","O","O","O","O","O"],"chunks":["B-NP","I-NP","B-VP","B-NP","I-NP","I-NP","I-NP","I-NP","O","B-NP","O","B-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nn"},{"source":2,"destination":1,"relation":"nsubj"},{"source":2,"destination":5,"relation":"dobj"},{"source":5,"destination":3,"relation":"num"},{"source":5,"destination":4,"relation":"nn"},{"source":5,"destination":11,"relation":"appos"},{"source":11,"destination":7,"relation":"nn"},{"source":11,"destination":9,"relation":"dep"}],"roots":[2]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nn"},{"source":2,"destination":1,"relation":"nsubj"},{"source":2,"destination":5,"relation":"dobj"},{"source":5,"destination":3,"relation":"num"},{"source":5,"destination":4,"relation":"nn"},{"source":5,"destination":11,"relation":"appos"},{"source":11,"destination":7,"relation":"nn"},{"source":11,"destination":9,"relation":"dep"}],"roots":[2]}}}]} """)

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
  val doc7 = jsonStringToDocument(""" {"sentences":[{"words":["JAK3","binds","to","MEK","and","RAS"],"startOffsets":[0,5,11,14,18,22],"endOffsets":[4,10,13,17,21,25],"tags":["NN","VBZ","TO","NN","CC","NN"],"lemmas":["jak3","bind","to","mek","and","ra"],"entities":["B-Gene_or_gene_product","O","O","B-Family","O","B-Family"],"chunks":["B-NP","B-VP","B-PP","B-NP","I-NP","I-NP"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"prep"},{"source":2,"destination":3,"relation":"pobj"},{"source":3,"destination":4,"relation":"cc"},{"source":3,"destination":5,"relation":"conj"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":3,"relation":"prep_to"},{"source":1,"destination":5,"relation":"prep_to"},{"source":3,"destination":5,"relation":"conj_and"}],"roots":[1]}}}]} """)

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
  val doc8 = Document(
    Array(
      Sentence(
        text8.split(" "),
        Array(0, 2, 4, 6, 8, 10, 12, 14, 16, 18),
        Array(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
      )
    )
  )

  "TokenPattern" should "handle repetition in lookbehind" in {
    val p = TokenPattern.compile("(?<= a (a b){3} a) b ")
    val results = p.findAllIn(0, doc8)
    results should have size (1)
    results.head.interval should have (
      'start (9),
      'end (10)
    )
  }

  //////////////////////////////////////////////////
  // test for variable-length lookbehind assertions
  //////////////////////////////////////////////////

  val posLookbehindPattern = """(?<= ^ [lemma=/(?i)^doctor$/] "Robert") Doback""""
  // match Doback if immediately preceded by "doctor robert" at the start of a sentence

  posLookbehindPattern should "allow for multi-token positive lookbehind assertions at the beginning of a sentence" in {
    val grammar = """
                    |- name: house_of_learned_doctors
                    |  priority: 1
                    |  type: token
                    |  label: LearnedDoctor
                    |  pattern: |
                    |    (?<= ^ [lemma=/(?i)^doctor$/] "Robert") Doback
                    |""".stripMargin

    val ee = ExtractorEngine(grammar)

    val text = "Doctor Robert Doback lost his dinosaur."
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["Doctor","Robert","Doback","lost","his","dinosaur","."],"startOffsets":[0,7,14,21,26,30,38],"endOffsets":[6,13,20,25,29,38,39],"tags":["NN","NNP","NNP","VBD","PRP$","NN","."],"lemmas":["doctor","Robert","Doback","lose","he","dinosaur","."],"entities":["O","PERSON","PERSON","O","O","O","O"],"norms":["O","O","O","O","O","O","O"],"chunks":["B-NP","I-NP","I-NP","B-VP","B-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":2,"destination":0,"relation":"nn"},{"source":2,"destination":1,"relation":"nn"},{"source":3,"destination":2,"relation":"nsubj"},{"source":3,"destination":5,"relation":"dobj"},{"source":3,"destination":6,"relation":"punct"},{"source":5,"destination":4,"relation":"poss"}],"roots":[3]},"stanford-collapsed":{"edges":[{"source":2,"destination":0,"relation":"nn"},{"source":2,"destination":1,"relation":"nn"},{"source":3,"destination":2,"relation":"nsubj"},{"source":3,"destination":5,"relation":"dobj"},{"source":3,"destination":6,"relation":"punct"},{"source":5,"destination":4,"relation":"poss"}],"roots":[3]}}}]} """)
    val results = ee.extractFrom(doc)

    results should have size (1)
    results.count(_ matches "LearnedDoctor") should equal(1)
  }

  // match person if it is not preceded by a sequence of titles (starting at beginning of sentence)
  val negLookbehindPattern = "(?<! ^ @Title+) @person:Person"

  negLookbehindPattern should "allow for variable-length negative lookbehind assertion" in {
    val grammar = """
                 |# our rule for capturing titles
                 |- name: title_rule
                 |  priority: 1
                 |  type: token
                 |  label: Title
                 |  pattern: |
                 |    [word=/(?i)^(herr|doktor|professor)$/]
                 |
                 |# our rule for capturing a Person NE
                 |- name: person_rule
                 |  priority: 1
                 |  type: token
                 |  label: Person
                 |  pattern: |
                 |    Faust
                 |
                 |- name: fellow_without_title_rule_1
                 |  priority: 2
                 |  type: token
                 |  label: TitlelessDude
                 |  pattern: |
                 |    (?<! ^ @Title+) @titleless:Person
                 |
                 |- name: fellow_without_title_rule_2
                 |  priority: 2
                 |  type: token
                 |  label: TitlelessDude
                 |  pattern: |
                 |    (?<! [word=/(?i)^(herr|doktor|professor)$/]+) @titleless:Person
                 |
                 |""".stripMargin


    val ee = ExtractorEngine(grammar)

    val text9a = "Herr Professor Doktor Faust, may I see you in my office?"
    val doc9a = jsonStringToDocument(""" {"sentences":[{"words":["Herr","Professor","Doktor","Faust",",","may","I","see","you","in","my","office","?"],"startOffsets":[0,5,15,22,27,29,33,35,39,43,46,49,55],"endOffsets":[4,14,21,27,28,32,34,38,42,45,48,55,56],"tags":["NNP","NNP","NNP","NNP",",","MD","PRP","VB","PRP","IN","PRP$","NN","."],"lemmas":["Herr","Professor","Doktor","Faust",",","may","I","see","you","in","my","office","?"],"entities":["B-Gene_or_gene_product","I-Gene_or_gene_product","I-Gene_or_gene_product","I-Gene_or_gene_product","O","O","O","O","O","O","O","O","O"],"chunks":["O","B-NP","I-NP","I-NP","O","O","B-NP","B-VP","B-NP","B-PP","B-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":3,"destination":0,"relation":"nn"},{"source":3,"destination":1,"relation":"nn"},{"source":3,"destination":2,"relation":"nn"},{"source":5,"destination":3,"relation":"nsubj"},{"source":5,"destination":6,"relation":"dobj"},{"source":5,"destination":7,"relation":"dep"},{"source":7,"destination":8,"relation":"dobj"},{"source":7,"destination":9,"relation":"prep"},{"source":9,"destination":11,"relation":"pobj"},{"source":11,"destination":10,"relation":"poss"}],"roots":[5]},"stanford-collapsed":{"edges":[{"source":3,"destination":0,"relation":"nn"},{"source":3,"destination":1,"relation":"nn"},{"source":3,"destination":2,"relation":"nn"},{"source":5,"destination":3,"relation":"nsubj"},{"source":5,"destination":6,"relation":"dobj"},{"source":5,"destination":7,"relation":"dep"},{"source":7,"destination":8,"relation":"dobj"},{"source":7,"destination":11,"relation":"prep_in"},{"source":11,"destination":10,"relation":"poss"}],"roots":[5]}}}]} """)
    val results9a = ee.extractFrom(doc9a)
    val titlelessDudeMentions9a = results9a.filter(_.label matches "TitlelessDude")
    // Should have (3) Title and (1) Person
    results9a should have size (4)
    titlelessDudeMentions9a should have size (0)

    val text9b = "Faust is a friend of mine."
    val doc9b = jsonStringToDocument(""" {"sentences":[{"words":["Faust","is","a","friend","of","mine","."],"startOffsets":[0,6,9,11,18,21,25],"endOffsets":[5,8,10,17,20,25,26],"tags":["NNP","VBZ","DT","NN","IN","NN","."],"lemmas":["Faust","be","a","friend","of","mine","."],"entities":["O","O","O","O","O","B-Family","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","B-PP","B-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":3,"destination":0,"relation":"nsubj"},{"source":3,"destination":1,"relation":"cop"},{"source":3,"destination":2,"relation":"det"},{"source":3,"destination":4,"relation":"prep"},{"source":4,"destination":5,"relation":"pobj"}],"roots":[3]},"stanford-collapsed":{"edges":[{"source":3,"destination":0,"relation":"nsubj"},{"source":3,"destination":1,"relation":"cop"},{"source":3,"destination":2,"relation":"det"},{"source":3,"destination":5,"relation":"prep_of"}],"roots":[3]}}}]} """)
    val results9b = ee.extractFrom(doc9b)
    val titlelessDudeMentions9b = results9b.filter(_.label matches "TitlelessDude")
    // Should have (1) Person and (1) TitlelessDude
    results9b should have size (2)
    titlelessDudeMentions9b should have size (1)
  }

  // negative lookbehind + pattern + negative lookahead
  val lookaroundPattern1 = "(?<! [tag=/^N/]+) [tag=/^N/] (?! [tag=/^N/]+)"
  lookaroundPattern1 should "find only single-noun NPs" in {

    val p = TokenPattern.compile(lookaroundPattern1)
    val text1 = "I'll have a cow eyeball smoothie, please."
    val doc1 = jsonStringToDocument(""" {"sentences":[{"words":["I","'ll","have","a","cow","eyeball","smoothie",",","please","."],"startOffsets":[0,1,5,10,12,16,24,32,34,40],"endOffsets":[1,4,9,11,15,23,32,33,40,41],"tags":["PRP","MD","VB","DT","NN","NN","NN",",","VB","."],"lemmas":["I","'ll","have","a","cow","eyeball","smoothie",",","please","."],"entities":["O","O","O","O","O","B-TissueType","O","O","O","O"],"chunks":["B-NP","B-VP","I-VP","B-NP","I-NP","I-NP","I-NP","O","B-VP","O"],"graphs":{"stanford-basic":{"edges":[{"source":2,"destination":0,"relation":"nsubj"},{"source":2,"destination":1,"relation":"aux"},{"source":2,"destination":4,"relation":"dobj"},{"source":4,"destination":3,"relation":"det"},{"source":4,"destination":5,"relation":"dep"},{"source":5,"destination":6,"relation":"dobj"},{"source":5,"destination":8,"relation":"dep"}],"roots":[2]},"stanford-collapsed":{"edges":[{"source":2,"destination":0,"relation":"nsubj"},{"source":2,"destination":1,"relation":"aux"},{"source":2,"destination":4,"relation":"dobj"},{"source":4,"destination":3,"relation":"det"},{"source":4,"destination":5,"relation":"dep"},{"source":5,"destination":6,"relation":"dobj"},{"source":5,"destination":8,"relation":"dep"}],"roots":[2]}}}]} """)
    val text2 = "What do you want in your smoothie?"
    val doc2 = jsonStringToDocument(""" {"sentences":[{"words":["What","do","you","want","in","your","smoothie","?"],"startOffsets":[0,5,8,12,17,20,25,33],"endOffsets":[4,7,11,16,19,24,33,34],"tags":["WP","VBP","PRP","VB","IN","PRP$","NN","."],"lemmas":["what","do","you","want","in","you","smoothie","?"],"entities":["O","O","O","O","O","O","O","O"],"chunks":["B-NP","O","B-NP","B-VP","B-PRT","B-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":3,"destination":0,"relation":"dobj"},{"source":3,"destination":1,"relation":"aux"},{"source":3,"destination":2,"relation":"nsubj"},{"source":3,"destination":4,"relation":"prep"},{"source":4,"destination":6,"relation":"pobj"},{"source":6,"destination":5,"relation":"poss"}],"roots":[3]},"stanford-collapsed":{"edges":[{"source":3,"destination":0,"relation":"dobj"},{"source":3,"destination":1,"relation":"aux"},{"source":3,"destination":2,"relation":"nsubj"},{"source":3,"destination":6,"relation":"prep_in"},{"source":6,"destination":5,"relation":"poss"}],"roots":[3]}}}]} """)
    val results1 = p.findAllIn(0, doc1)
    val results2 = p.findAllIn(0, doc2)

    results1 should have size (0)
    results2 should have size (1)
    results2.head.interval should have (
      'start (6),
      'end (7)
    )
  }

  // positive lookbehind in negative lookbehind
  // match "black cat" if it is not preceded by token "mangy" that is preceded by token "a"
  val lookaroundPattern2 = "(?<! (?<= a) mangy) black cat"

  lookaroundPattern2 should "not match \"black cat\" in \"a mangy black cat\"" in {
    val p = TokenPattern.compile(lookaroundPattern2)
    val text = "I glimpsed a mangy black cat in the moonlight."
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["I","glimpsed","a","mangy","black","cat","in","the","moonlight","."],"startOffsets":[0,2,11,13,19,25,29,32,36,45],"endOffsets":[1,10,12,18,24,28,31,35,45,46],"tags":["PRP","VBD","DT","JJ","JJ","NN","IN","DT","NN","."],"lemmas":["I","glimpse","a","mangy","black","cat","in","the","moonlight","."],"entities":["O","O","O","O","O","B-Gene_or_gene_product","O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","I-NP","I-NP","B-PP","B-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":1,"destination":6,"relation":"prep"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"amod"},{"source":5,"destination":4,"relation":"amod"},{"source":6,"destination":8,"relation":"pobj"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":1,"destination":8,"relation":"prep_in"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"amod"},{"source":5,"destination":4,"relation":"amod"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]}}}]} """)
    val results = p.findAllIn(0, doc)
    results should have size (0)
  }

  it should "match \"black cat\" in \"the mangy black cat\", and \"the curious black cat\"" in {
    val p = TokenPattern.compile(lookaroundPattern2)
    val text1 = "I glimpsed the mangy black cat in the moonlight."
    val doc1 = jsonStringToDocument(""" {"sentences":[{"words":["I","glimpsed","the","mangy","black","cat","in","the","moonlight","."],"startOffsets":[0,2,11,15,21,27,31,34,38,47],"endOffsets":[1,10,14,20,26,30,33,37,47,48],"tags":["PRP","VBD","DT","JJ","JJ","NN","IN","DT","NN","."],"lemmas":["I","glimpse","the","mangy","black","cat","in","the","moonlight","."],"entities":["O","O","O","O","O","B-Gene_or_gene_product","O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","I-NP","I-NP","B-PP","B-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":1,"destination":6,"relation":"prep"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"amod"},{"source":5,"destination":4,"relation":"amod"},{"source":6,"destination":8,"relation":"pobj"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":1,"destination":8,"relation":"prep_in"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"amod"},{"source":5,"destination":4,"relation":"amod"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]}}}]} """)
    val text2 = "I glimpsed the curious black cat in the moonlight."
    val doc2 = jsonStringToDocument(""" {"sentences":[{"words":["I","glimpsed","the","curious","black","cat","in","the","moonlight","."],"startOffsets":[0,2,11,15,23,29,33,36,40,49],"endOffsets":[1,10,14,22,28,32,35,39,49,50],"tags":["PRP","VBD","DT","JJ","JJ","NN","IN","DT","NN","."],"lemmas":["I","glimpse","the","curious","black","cat","in","the","moonlight","."],"entities":["O","O","O","O","O","B-Gene_or_gene_product","O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","I-NP","I-NP","B-PP","B-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":1,"destination":6,"relation":"prep"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"amod"},{"source":5,"destination":4,"relation":"amod"},{"source":6,"destination":8,"relation":"pobj"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":1,"destination":8,"relation":"prep_in"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"amod"},{"source":5,"destination":4,"relation":"amod"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]}}}]} """)
    val results1 = p.findAllIn(0, doc1)
    val results2 = p.findAllIn(0, doc2)

    // matches b/c neg lookbehind req. contained pattern to be false
    results1 should have size (1)
    results1.head.interval should have (
      'start (4),
      'end (6)
    )
    results2 should have size (1)
    // matches b/c neg lookbehind req. contained pattern to be false
    results2.head.interval should have (
      'start (4),
      'end (6)
    )
  }

  it should "not match \"the curious blue cat\"" in {
    val text1 = "I glimpsed the curious blue cat in the moonlight."
    val doc1 = jsonStringToDocument(""" {"sentences":[{"words":["I","glimpsed","the","curious","blue","cat","in","the","moonlight","."],"startOffsets":[0,2,11,15,23,28,32,35,39,48],"endOffsets":[1,10,14,22,27,31,34,38,48,49],"tags":["PRP","VBD","DT","JJ","JJ","NN","IN","DT","NN","."],"lemmas":["I","glimpse","the","curious","blue","cat","in","the","moonlight","."],"entities":["O","O","O","O","O","B-Gene_or_gene_product","O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","I-NP","I-NP","B-PP","B-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":1,"destination":6,"relation":"prep"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"amod"},{"source":5,"destination":4,"relation":"amod"},{"source":6,"destination":8,"relation":"pobj"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":1,"destination":8,"relation":"prep_in"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"amod"},{"source":5,"destination":4,"relation":"amod"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]}}}]} """)
    val p = TokenPattern.compile(lookaroundPattern2)
    val results1 = p.findAllIn(0, doc1)
    results1 should have size (0)
  }

  // negative lookbehind in negative lookbehind
  // only match "black cat" if it is not preceded by token "mangy" that is not preceded by the token "the"
  val lookaroundPattern3 = "(?<! (?<! the) mangy) black cat"

  lookaroundPattern3 should "not match \"a mangy black cat\", \"a mangy blue cat\", or \"the mangy black cat\"" in {
    val p = TokenPattern.compile(lookaroundPattern3)
    val text1 = "I glimpsed a mangy black cat in the moonlight."
    val doc1 = jsonStringToDocument(""" {"sentences":[{"words":["I","glimpsed","a","mangy","black","cat","in","the","moonlight","."],"startOffsets":[0,2,11,13,19,25,29,32,36,45],"endOffsets":[1,10,12,18,24,28,31,35,45,46],"tags":["PRP","VBD","DT","JJ","JJ","NN","IN","DT","NN","."],"lemmas":["I","glimpse","a","mangy","black","cat","in","the","moonlight","."],"entities":["O","O","O","O","O","B-Gene_or_gene_product","O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","I-NP","I-NP","B-PP","B-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":1,"destination":6,"relation":"prep"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"amod"},{"source":5,"destination":4,"relation":"amod"},{"source":6,"destination":8,"relation":"pobj"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":1,"destination":8,"relation":"prep_in"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"amod"},{"source":5,"destination":4,"relation":"amod"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]}}}]} """)
    val text2 = "I glimpsed a mangy blue cat in the moonlight."
    val doc2 = jsonStringToDocument(""" {"sentences":[{"words":["I","glimpsed","a","mangy","blue","cat","in","the","moonlight","."],"startOffsets":[0,2,11,13,19,24,28,31,35,44],"endOffsets":[1,10,12,18,23,27,30,34,44,45],"tags":["PRP","VBD","DT","NN","JJ","NN","IN","DT","NN","."],"lemmas":["I","glimpse","a","mangy","blue","cat","in","the","moonlight","."],"entities":["O","O","O","O","O","B-Gene_or_gene_product","O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","I-NP","I-NP","B-PP","B-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"nn"},{"source":5,"destination":4,"relation":"amod"},{"source":5,"destination":6,"relation":"prep"},{"source":6,"destination":8,"relation":"pobj"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"nn"},{"source":5,"destination":4,"relation":"amod"},{"source":5,"destination":8,"relation":"prep_in"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]}}}]} """)
    val results1 = p.findAllIn(0, doc1)
    val results2 = p.findAllIn(0, doc2)
    results1 should have size (0)
    results2 should have size (0)
  }

  it should "match \"black cat\" in \"the mangy black cat\"" in {
    val p = TokenPattern.compile(lookaroundPattern3)
    val text = "I glimpsed the mangy black cat in the moonlight."
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["I","glimpsed","the","mangy","black","cat","in","the","moonlight","."],"startOffsets":[0,2,11,15,21,27,31,34,38,47],"endOffsets":[1,10,14,20,26,30,33,37,47,48],"tags":["PRP","VBD","DT","JJ","JJ","NN","IN","DT","NN","."],"lemmas":["I","glimpse","the","mangy","black","cat","in","the","moonlight","."],"entities":["O","O","O","O","O","B-Gene_or_gene_product","O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","I-NP","I-NP","B-PP","B-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":1,"destination":6,"relation":"prep"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"amod"},{"source":5,"destination":4,"relation":"amod"},{"source":6,"destination":8,"relation":"pobj"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":1,"destination":8,"relation":"prep_in"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"amod"},{"source":5,"destination":4,"relation":"amod"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]}}}]} """)
    val results = p.findAllIn(0, doc)
    results should have size (1)
    results.head.interval should have (
      'start (4),
      'end (6)
    )
  }

  // negative lookbehind in negative lookbehind
  it should "match \"black cat\" in \"a curious black cat\"" in {
    val p = TokenPattern.compile(lookaroundPattern3)
    val text1 = "I glimpsed a curious black cat in the moonlight."
    val doc1 = jsonStringToDocument(""" {"sentences":[{"words":["I","glimpsed","a","curious","black","cat","in","the","moonlight","."],"startOffsets":[0,2,11,13,21,27,31,34,38,47],"endOffsets":[1,10,12,20,26,30,33,37,47,48],"tags":["PRP","VBD","DT","JJ","JJ","NN","IN","DT","NN","."],"lemmas":["I","glimpse","a","curious","black","cat","in","the","moonlight","."],"entities":["O","O","O","O","O","B-Gene_or_gene_product","O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","I-NP","I-NP","B-PP","B-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":1,"destination":6,"relation":"prep"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"amod"},{"source":5,"destination":4,"relation":"amod"},{"source":6,"destination":8,"relation":"pobj"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":5,"relation":"dobj"},{"source":1,"destination":8,"relation":"prep_in"},{"source":5,"destination":2,"relation":"det"},{"source":5,"destination":3,"relation":"amod"},{"source":5,"destination":4,"relation":"amod"},{"source":8,"destination":7,"relation":"det"}],"roots":[1]}}}]} """)
    val results1 = p.findAllIn(0, doc1)
    results1 should have size (1)
    // matches b/c neg lookbehind req. contained pattern to be false
    results1.head.interval should have (
      'start (4),
      'end (6)
    )
  }

  // test mention arg patterns

  val argPattern1 = "@Binding.theme @trig1:Binding.trigger @Binding.theme @trig2:Binding.trigger"
  val exampleSentence = "MEK bound Ras binds with Kras"
  argPattern1 should s"""match "$exampleSentence"""" in {
    val p = TokenPattern.compile(argPattern1)
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["MEK","bound","Ras","binds","with","Kras"],"startOffsets":[0,4,10,14,20,25],"endOffsets":[3,9,13,19,24,29],"tags":["NN","VBD","NN","VBZ","IN","NNP"],"lemmas":["mek","bind","ra","bind","with","Kras"],"entities":["B-Family","O","B-Family","O","O","B-Gene_or_gene_product"],"chunks":["B-NP","B-VP","B-NP","B-VP","B-PP","B-NP"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":3,"relation":"ccomp"},{"source":3,"destination":2,"relation":"nsubj"},{"source":3,"destination":4,"relation":"prep"},{"source":4,"destination":5,"relation":"pobj"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":3,"relation":"ccomp"},{"source":3,"destination":2,"relation":"nsubj"},{"source":3,"destination":5,"relation":"prep_with"}],"roots":[1]}}}]} """)

    // proteins
    val prot1 = new TextBoundMention("Protein", Interval(0), 0, doc, false, "<MANUAL>")
    val prot2 = new TextBoundMention("Protein", Interval(2), 0, doc, false, "<MANUAL>")
    val prot3 = new TextBoundMention("Protein", Interval(5), 0, doc, false, "<MANUAL>")
    // triggers
    val trigger1 = new TextBoundMention("BindingTrigger", Interval(1), 0, doc, false, "<MANUAL>")
    val trigger2 = new TextBoundMention("BindingTrigger", Interval(3), 0, doc, false, "<MANUAL>")

    // initialize state
    val mentions = Seq(
      // proteins
      prot1, prot2, prot3,
      // binding triggers
      trigger1, trigger2,
      // first binding
      new EventMention(
        label = "Binding",
        trigger = trigger1,
        arguments = Map("theme" -> Seq(prot1, prot2)),
        sentence = 0,
        document = doc,
        keep = true,
        foundBy = "<MANUAL>"
      ),
      // second binding
      new EventMention(
        label = "Binding",
        trigger = trigger2,
        arguments = Map("theme" -> Seq(prot2, prot3)),
        sentence = 0,
        document = doc,
        keep = true,
        foundBy = "<MANUAL>"
      )
    )

    val state = State(mentions)
    val results = p.findAllIn(0, doc, state)

    results should have size (1)
    results.head.interval should have (
      'start (0),
      'end (4)
    )

  }

  val argPattern2 = "@Binding.theme [mention=Binding.trigger & tag=VBD] @Binding.theme [mention=Binding.trigger & tag=VBZ]"
  argPattern2 should s"""match "$exampleSentence"""" in {
    val p = TokenPattern.compile(argPattern2)
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["MEK","bound","Ras","binds","with","Kras"],"startOffsets":[0,4,10,14,20,25],"endOffsets":[3,9,13,19,24,29],"tags":["NN","VBD","NN","VBZ","IN","NNP"],"lemmas":["mek","bind","ra","bind","with","Kras"],"entities":["B-Family","O","B-Family","O","O","B-Gene_or_gene_product"],"chunks":["B-NP","B-VP","B-NP","B-VP","B-PP","B-NP"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":3,"relation":"ccomp"},{"source":3,"destination":2,"relation":"nsubj"},{"source":3,"destination":4,"relation":"prep"},{"source":4,"destination":5,"relation":"pobj"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":3,"relation":"ccomp"},{"source":3,"destination":2,"relation":"nsubj"},{"source":3,"destination":5,"relation":"prep_with"}],"roots":[1]}}}]} """)

    // proteins
    val prot1 = new TextBoundMention("Protein", Interval(0), 0, doc, false, "<MANUAL>")
    val prot2 = new TextBoundMention("Protein", Interval(2), 0, doc, false, "<MANUAL>")
    val prot3 = new TextBoundMention("Protein", Interval(5), 0, doc, false, "<MANUAL>")
    // triggers
    val trigger1 = new TextBoundMention("BindingTrigger", Interval(1), 0, doc, false, "<MANUAL>")
    val trigger2 = new TextBoundMention("BindingTrigger", Interval(3), 0, doc, false, "<MANUAL>")

    // initialize state
    val mentions = Seq(
      // proteins
      prot1, prot2, prot3,
      // binding triggers
      trigger1, trigger2,
      // first binding
      new EventMention(
        label = "Binding",
        trigger = trigger1,
        arguments = Map("theme" -> Seq(prot1, prot2)),
        sentence = 0,
        document = doc,
        keep = true,
        foundBy = "<MANUAL>"
      ),
      // second binding
      new EventMention(
        label = "Binding",
        trigger = trigger2,
        arguments = Map("theme" -> Seq(prot2, prot3)),
        sentence = 0,
        document = doc,
        keep = true,
        foundBy = "<MANUAL>"
      )
    )

    val state = State(mentions)
    val results = p.findAllIn(0, doc, state)

    results should have size (1)
    results.head.interval should have (
      'start (0),
      'end (4)
    )

  }

  "overlapping mentions" should "all be considered for matching" in {
    // ASPP1 and ASPP2 are phosphorylated in response to EGFR
    val doc = jsonStringToDocument("""{"sentences":[{"words":["ASPP1","and","ASPP2","are","phosphorylated","in","response","to","EGFR"],"startOffsets":[0,6,10,16,20,35,38,47,50],"endOffsets":[5,9,15,19,34,37,46,49,54],"tags":["NN","CC","NN","VBP","VBN","IN","NN","TO","NN"],"lemmas":["aspp1","and","aspp2","be","phosphorylate","in","response","to","egfr"],"entities":["B-Gene_or_gene_product","O","B-Gene_or_gene_product","O","O","O","O","O","B-Gene_or_gene_product"],"chunks":["B-NP","I-NP","I-NP","B-VP","I-VP","B-PP","B-NP","B-PP","B-NP"],"graphs":{"stanford-basic":{"edges":[{"source":0,"destination":1,"relation":"cc"},{"source":0,"destination":2,"relation":"conj"},{"source":4,"destination":0,"relation":"nsubjpass"},{"source":4,"destination":3,"relation":"auxpass"},{"source":4,"destination":5,"relation":"prep"},{"source":4,"destination":7,"relation":"prep"},{"source":5,"destination":6,"relation":"pobj"},{"source":7,"destination":8,"relation":"pobj"}],"roots":[4]},"stanford-collapsed":{"edges":[{"source":0,"destination":2,"relation":"conj_and"},{"source":4,"destination":0,"relation":"nsubjpass"},{"source":4,"destination":2,"relation":"nsubjpass"},{"source":4,"destination":3,"relation":"auxpass"},{"source":4,"destination":6,"relation":"prep_in"},{"source":4,"destination":8,"relation":"prep_to"}],"roots":[4]}}}]}""")

    // proteins
    val prot1 = new TextBoundMention("Protein", Interval(0), 0, doc, false, "<MANUAL>")
    val prot2 = new TextBoundMention("Protein", Interval(2), 0, doc, false, "<MANUAL>")
    val prot3 = new TextBoundMention("Protein", Interval(8), 0, doc, false, "<MANUAL>")
    // triggers
    val trigger1 = new TextBoundMention("PhosphoTrigger", Interval(4), 0, doc, false, "<MANUAL>")
    // state
    val mentions = Seq(
      // proteins
      prot1, prot2, prot3,
      // binding triggers
      trigger1,
      // first phosphorylation
      new EventMention(
        label = "Phosphorylation",
        trigger = trigger1,
        arguments = Map("theme" -> Seq(prot1)),
        sentence = 0,
        document = doc,
        keep = false,
        foundBy = "<MANUAL>"
      ),
      // second phosphorylation
      new EventMention(
        label = "Phosphorylation",
        trigger = trigger1,
        arguments = Map("theme" -> Seq(prot2)),
        sentence = 0,
        document = doc,
        keep = false,
        foundBy = "<MANUAL>"
      )
    )

    val state = State(mentions)
    val p = TokenPattern.compile("@Phosphorylation in response to @Protein")
    val results = p.findAllIn(0, doc, state)

    results should have size (2)
    val Seq(p1, p2) = results

    p1.interval should have (
      'start (0),
      'end (9)
    )

    p2.interval should have (
      'start (2),
      'end (9)
    )

  }

  it should "all be considered for matching here too" in {
    // ASPP1 and ASPP2 complex are phosphorylated in response to EGFR
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["ASPP1","and","ASPP2","complex","are","phosphorylated","in","response","to","EGFR"],"startOffsets":[0,6,10,16,24,28,43,46,55,58],"endOffsets":[5,9,15,23,27,42,45,54,57,62],"tags":["NN","CC","NN","NN","VBP","VBN","IN","NN","TO","NN"],"lemmas":["aspp1","and","aspp2","complex","be","phosphorylate","in","response","to","egfr"],"entities":["B-Gene_or_gene_product","O","B-Gene_or_gene_product","O","O","O","O","O","O","B-Gene_or_gene_product"],"chunks":["B-NP","I-NP","I-NP","I-NP","B-VP","I-VP","B-PP","B-NP","B-PP","B-NP"],"graphs":{"stanford-basic":{"edges":[{"source":0,"destination":1,"relation":"cc"},{"source":0,"destination":2,"relation":"conj"},{"source":3,"destination":0,"relation":"nn"},{"source":5,"destination":3,"relation":"nsubjpass"},{"source":5,"destination":4,"relation":"auxpass"},{"source":5,"destination":6,"relation":"prep"},{"source":5,"destination":8,"relation":"prep"},{"source":6,"destination":7,"relation":"pobj"},{"source":8,"destination":9,"relation":"pobj"}],"roots":[5]},"stanford-collapsed":{"edges":[{"source":0,"destination":2,"relation":"conj_and"},{"source":3,"destination":0,"relation":"nn"},{"source":3,"destination":2,"relation":"nn"},{"source":5,"destination":3,"relation":"nsubjpass"},{"source":5,"destination":4,"relation":"auxpass"},{"source":5,"destination":7,"relation":"prep_in"},{"source":5,"destination":9,"relation":"prep_to"}],"roots":[5]}}}]} """)

    // proteins
    val prot1 = new TextBoundMention("Protein", Interval(0), 0, doc, false, "<MANUAL>")
    val prot2 = new TextBoundMention("Protein", Interval(2), 0, doc, false, "<MANUAL>")
    val prot3 = new TextBoundMention("Protein", Interval(0, 4), 0, doc, false, "<MANUAL>")
    val prot4 = new TextBoundMention("Protein", Interval(9), 0, doc, false, "<MANUAL>")
    // triggers
    val trigger1 = new TextBoundMention("PhosphoTrigger", Interval(5), 0, doc, false, "<MANUAL>")
    // state
    val mentions = Seq(
      // proteins
      prot1, prot2, prot3, prot4,
      // binding triggers
      trigger1,
      // first phosphorylation
      new EventMention(
        label = "Phosphorylation",
        trigger = trigger1,
        arguments = Map("theme" -> Seq(prot1)),
        sentence = 0,
        document = doc,
        keep = false,
        foundBy = "<MANUAL>"
      ),
      // second phosphorylation
      new EventMention(
        label = "Phosphorylation",
        trigger = trigger1,
        arguments = Map("theme" -> Seq(prot2)),
        sentence = 0,
        document = doc,
        keep = false,
        foundBy = "<MANUAL>"
      ),
      // third phosphorylation
      new EventMention(
        label = "Phosphorylation",
        trigger = trigger1,
        arguments = Map("theme" -> Seq(prot3)),
        sentence = 0,
        document = doc,
        keep = false,
        foundBy = "<MANUAL>"
      )
    )

    val state = State(mentions)
    val p = TokenPattern.compile("@Phosphorylation in response to @Protein")
    val results = p.findAllIn(0, doc, state)

    results should have size (3)
    val Seq(p1, p2, p3) = results

    p1.interval should have (
      'start (0),
      'end (10)
    )

    p2.interval should have (
      'start (0),
      'end (10)
    )

    p3.interval should have (
      'start (2),
      'end (10)
    )

  }

}
