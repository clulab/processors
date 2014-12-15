package edu.arizona.sista.matcher

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor

object myActions extends Actions {
  def mkTextBoundMention(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    Seq(new TextBoundMention(label, mention("--GLOBAL--").head, sent, doc, ruleName))
  }

  def mkConversion(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)
    val theme = state.mentionsFor(sent, mention("theme").head.start, "Protein").head
    val cause = if (mention contains "cause") state.mentionsFor(sent, mention("cause").head.start, "Protein").headOption else None
    val args = if (cause.isDefined) Map("Theme" -> Seq(theme), "Cause" -> Seq(cause.get)) else Map("Theme" -> Seq(theme))
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
    Seq(trigger, event)
  }

  def mkRegulation(label: String, mention: Map[String, Seq[Interval]], sent: Int, doc: Document, ruleName: String, state: State): Seq[Mention] = {
    val trigger = new TextBoundMention(label, mention("trigger").head, sent, doc, ruleName)
    val cause = state.mentionsFor(sent, mention("cause").head.start, "Protein").head
    val theme = state.mentionsFor(sent, mention("theme").head.start, Seq("Phosphorylation", "Ubiquitination")).find(_.isInstanceOf[EventMention]).get
    val args = Map("Theme" -> Seq(theme), "Cause" -> Seq(cause))
    val event = new EventMention(label, trigger, args, sent, doc, ruleName)
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
                 |    trigger: [word=/^phospho/ & tag=/^VB/]
                 |    theme: dobj [mention=Protein]
                 |    cause: nsubj
                 |
                 |- name: rule3
                 |  label: Ubiquitination
                 |  priority: 2
                 |  type: dependency
                 |  action: mkConversion
                 |  pattern: |
                 |    trigger: ubiquitination
                 |    theme: prep_of
                 |
                 |- name: rule4
                 |  label: DownRegulation
                 |  priority: 3
                 |  type: dependency
                 |  action: mkRegulation
                 |  pattern: |
                 |    trigger: [lemma=inhibit]
                 |    theme: dobj [mention=/^Ubi/ & !mention=Phosphorylation]
                 |    cause: nsubj
                 |""".stripMargin

  val extractor = new ExtractorEngine(rules, myActions)

  val proc = new BioNLPProcessor
  val doc = proc annotate text
  doc.sentences.head.entities = Some(entities)

  val mentions = extractor extractFrom doc

  def words(m: Mention) =
    m.document.sentences(m.sentence).words.slice(m.tokenInterval.start, m.tokenInterval.end).mkString(" ")

  for (m <- mentions) {
    m match {
      case m: EventMention =>
        println(m.label)
        println(s"string = ${words(m)}")
        println(s"trigger = ${words(m.trigger)}")
        m.arguments foreach {
          case (k,vs) => for (v <- vs) println(s"$k = '${words(v)}'")
        }
        println("=" * 72)
      case _ => ()
    }
  }
}
