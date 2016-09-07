package org.clulab.odin

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.struct.Interval
import org.scalatest._

class TestDependencyPattern extends FlatSpec with Matchers {
  val proc = new FastNLPProcessor

  "DependencyPattern" should "support multiline patterns" in {

    val text = "I saw Kermit at the pond."
    val doc = proc.annotate(text)

    val rule = """
      |- name: testRule
      |  label: Sight
      |  pattern: |
      |    trigger = saw
      |    participants:Entity+ = # both participants (+)
      |      nsubj                # the nominal subject
      |        |                  # and
      |      dobj                 # the direct object
      |    location:Place = prep_at
      |""".stripMargin

    val mentions = Seq(
      new TextBoundMention("Entity", Interval(0), 0, doc, false, "<test>"),
      new TextBoundMention("Entity", Interval(2), 0, doc, false, "<test>"),
      new TextBoundMention("Place", Interval(5), 0, doc, false, "<test>")
    )

    val state = State(mentions)
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc, state)
    results should have size (1)
    results.head.arguments should contain key ("participants")
    results.head.arguments should contain key ("location")

  }

  it should "support quantified arguments" in {

    val text = "The binding of Ras, TGFBR1, MEK and TGFBR2."
    val doc = proc.annotate(text)
    //println(doc.sentences.head.dependencies.get)

    val rule = """
      |- name: testRule
      |  label: Binding
      |  pattern: |
      |    trigger = binding
      |    theme:Protein{2} = prep_of conj?
      |""".stripMargin

    val mentions = Seq(
      new TextBoundMention("Protein", Interval(3), 0, doc, false, "<test>"),
      new TextBoundMention("Protein", Interval(5), 0, doc, false, "<test>"),
      new TextBoundMention("Protein", Interval(7), 0, doc, false, "<test>"),
      new TextBoundMention("Protein", Interval(9), 0, doc, false, "<test>")
    )

    val state = State(mentions)
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc, state)

    results should have size (6)

  }

  it should "handle multitoken triggers" in {
    val text = "We found that prolonged expression of active Ras resulted in upregulation of the MKP3 gene."
    val doc = proc.annotate(text)

    val rule = """
      |- name: Positive_activation_syntax_8_verb
      |  priority: 2
      |  label: Positive_activation
      |  pattern: |
      |    trigger = [lemma=result] in [word=/(?i)^(upregul)/]
      |    controlled:Protein = prep_of nn
      |    controller:Protein = nsubj prep_of
      |""".stripMargin

    val mentions = Seq(
      new TextBoundMention("Protein", Interval(7), 0, doc, false, "<test>"),
      new TextBoundMention("Protein", Interval(13), 0, doc, false, "<test>")
    )

    val state = State(mentions)
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc, state)

    results.filter(_ matches "Positive_activation") should have size (1)

  }

}
