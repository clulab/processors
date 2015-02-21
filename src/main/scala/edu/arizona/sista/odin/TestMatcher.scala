package edu.arizona.sista.odin

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.odin._

object myActions extends Actions {
  def mkTextBoundMention(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State, keep: Boolean): Seq[Mention] = {
    Seq(new TextBoundMention(label, mention("--GLOBAL--").head, sent, doc, keep, ruleName))
  }

  def mkConversion(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State, keep: Boolean): Seq[Mention] = {
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, keep, ruleName)
    val theme = state.mentionsFor(sent, mention("theme").head.start, "Protein").head
    val cause = if (mention contains "cause") state.mentionsFor(sent, mention("cause").head.start, "Protein").headOption else None
    val args = if (cause.isDefined) Map("Theme" -> Seq(theme), "Cause" -> Seq(cause.get)) else Map("Theme" -> Seq(theme))
    val event = new EventMention(label, trigger, args, sent, doc, keep, ruleName)
    Seq(trigger, event)
  }

  def mkRegulation(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State, keep: Boolean): Seq[Mention] = {
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, keep, ruleName)
    val cause = state.mentionsFor(sent, mention("cause").head.start, "Protein").head
    val theme = state.mentionsFor(sent, mention("theme").head.start, Seq("Phosphorylation", "Ubiquitination")).find(_.isInstanceOf[EventMention]).get
    val args = Map("Theme" -> Seq(theme), "Cause" -> Seq(cause))
    val event = new EventMention(label, trigger, args, sent, doc, keep, ruleName)
    Seq(trigger, event)
  }
}

object TestMatcher extends App {
  val text = "TGFBR2 phosphorylates peri-kappa B and inhibits the ubiquitination of SMAD3."
  val entities = Array("B-Protein", "O", "B-Protein", "I-Protein", "O", "O", "O", "O", "O", "B-Protein", "O")

  val rules = """|- name: rule1
                 |  label: Protein
                 |  priority: 1
                 |  type: token
                 |  action: mkTextBoundMention
                 |  pattern: |
                 |    [entity='B-Protein'] [entity='I-Protein']*
                 |
                 |- name: rule2
                 |  label: Phosphorylation
                 |  priority: 2
                 |  type: dependency
                 |  action: mkConversion
                 |  pattern: |
                 |    trigger = [word=/^phospho/ & tag=/^VB/]
                 |    theme: Protein = dobj [mention=Protein]
                 |    cause: Protein = nsubj
                 |
                 |- name: rule3
                 |  label: Ubiquitination
                 |  priority: 2
                 |  type: dependency
                 |  action: mkConversion
                 |  pattern: |
                 |    trigger = ubiquitination
                 |    theme: Protein = prep_of
                 |
                 |- name: rule4
                 |  label: DownRegulation
                 |  priority: 3
                 |  type: dependency
                 |  action: mkRegulation
                 |  pattern: |
                 |    trigger = [lemma=inhibit]
                 |    theme: Ubiquitination = dobj [mention=/^Ubi/ & !mention=Phosphorylation]
                 |    cause: Protein = nsubj
                 |""".stripMargin

  val extractor = new ExtractorEngine(rules, myActions)

  val proc = new BioNLPProcessor
  val doc = proc annotate text
  doc.sentences.head.entities = Some(entities)

  val mentions = extractor extractFrom doc

  for (m <- mentions) {
    m match {
      case m: EventMention =>
        println(m.label)
        println(s"string = ${m.text}")
        println(s"trigger = ${m.trigger.text}")
        m.arguments foreach {
          case (k,vs) => for (v <- vs) println(s"$k = ${v.text}")
        }
        println("=" * 72)
      case _ => ()
    }
  }
}
