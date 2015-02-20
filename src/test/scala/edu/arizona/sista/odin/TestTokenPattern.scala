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
    assert(results.size == 1)
  }

  text2 should "contain one match" in {
    val doc = proc annotate text2
    val p = TokenPattern.compile("@Phosphorylation and inhibits")
    val mentions = Seq(new TextBoundMention("Phosphorylation", Interval(0, 4), 0, doc, true, "<MANUAL>"),
                       new TextBoundMention("Phosphorylation", Interval(0, 7), 0, doc, true, "<MANUAL>"))
    val state = State(mentions)
    val results = p.findAllIn(0, doc, state)
    assert(results.size == 1)
  }

  text3 should "contain two matches" in {
    val doc = proc annotate text3
    val p = TokenPattern.compile("@Phosphorylation and inhibits")
    val mentions = Seq(new TextBoundMention("Phosphorylation", Interval(0, 9), 0, doc, true, "<MANUAL>"),
                       new TextBoundMention("Phosphorylation", Interval(0, 9), 0, doc, true, "<MANUAL>"))
    val state = State(mentions)
    val results = p.findAllIn(0, doc, state)
    assert(results.size == 2)
  }
}
