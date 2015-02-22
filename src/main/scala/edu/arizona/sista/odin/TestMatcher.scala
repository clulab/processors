package edu.arizona.sista.odin

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.odin._

object myActions extends Actions {
  def defaultAction(mention: Mention, state: State): Seq[Mention] = Seq(mention)
}

object TestMatcher extends App {
  val text = "TGFBR2 phosphorylates peri-kappa B and inhibits the ubiquitination of SMAD3."
  val entities = Array("B-Protein", "O", "B-Protein", "I-Protein", "O", "O", "O", "O", "O", "B-Protein", "O")

  val rules = """|- name: rule1
                 |  label: Protein
                 |  priority: 1
                 |  type: token
                 |  action: defaultAction
                 |  pattern: |
                 |    [entity='B-Protein'] [entity='I-Protein']*
                 |
                 |- name: rule2
                 |  label: Phosphorylation
                 |  priority: 2
                 |  type: dependency
                 |  action: defaultAction
                 |  pattern: |
                 |    trigger = [word=/^phospho/ & tag=/^VB/]
                 |    theme: Protein = dobj [mention=Protein]
                 |    cause: Protein = nsubj
                 |
                 |- name: rule3
                 |  label: Ubiquitination
                 |  priority: 2
                 |  type: dependency
                 |  action: defaultAction
                 |  pattern: |
                 |    trigger = ubiquitination
                 |    theme: Protein = prep_of
                 |
                 |- name: rule4
                 |  label: DownRegulation
                 |  priority: 3
                 |  type: dependency
                 |  action: defaultAction
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
