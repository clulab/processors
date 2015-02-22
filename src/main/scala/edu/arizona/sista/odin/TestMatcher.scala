package edu.arizona.sista.odin

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.odin._

object TestMatcher extends App {
  val text = "TGFBR2 phosphorylates peri-kappa B and inhibits the ubiquitination of SMAD3."
  val entities = Array("B-Protein", "O", "B-Protein", "I-Protein", "O", "O", "O", "O", "O", "B-Protein", "O")

  val rules = """|- name: rule1
                 |  label: Protein
                 |  type: token
                 |  pattern: |
                 |    [entity='B-Protein'] [entity='I-Protein']*
                 |
                 |- name: rule2
                 |  label: Phosphorylation
                 |  pattern: |
                 |    Trigger = [word=/^phospho/ & tag=/^VB/]
                 |    theme: Protein = dobj
                 |    cause: Protein = nsubj
                 |
                 |- name: rule3
                 |  label: Ubiquitination
                 |  pattern: |
                 |    triggER = ubiquitination
                 |    theme: Protein = prep_of
                 |
                 |- name: rule4
                 |  label: DownRegulation
                 |  pattern: |
                 |    trigger = [lemma=inhibit]
                 |    theme: Ubiquitination = dobj
                 |    cause: Protein = nsubj
                 |
                 |- name: rule5
                 |  label: XXXtestXXX
                 |  type: token
                 |  pattern: |
                 |    @theme:Phosphorylation and (?<TriGgEr> inhibits) the @cause:Ubiquitination
                 |""".stripMargin

  val extractor = new ExtractorEngine(rules)

  val proc = new BioNLPProcessor
  val doc = proc annotate text
  doc.sentences.head.entities = Some(entities)

  val mentions = extractor extractFrom doc

  for (m <- mentions) {
    m match {
      case m: EventMention =>
        println("EventMention")
        println(m.label)
        println(s"string = ${m.text}")
        println(s"trigger = ${m.trigger.text}")
        m.arguments foreach {
          case (k,vs) => for (v <- vs) println(s"$k = ${v.text}")
        }
        println("=" * 72)
      case m: RelationMention =>
        println("RelationMention")
        println(m.label)
        println(s"string = ${m.text}")
        m.arguments foreach {
          case (k,vs) => for (v <- vs) println(s"$k = ${v.text}")
        }
        println("=" * 72)
      case _ => ()
    }
  }
}
