package edu.arizona.sista.odin

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.odin._

object TestMatcher extends App {
  // two example sentences with manually defined named entity tags in IOB notation
  val sentence0 = "TGFBR2 phosphorylates peri-kappa B and inhibits the ubiquitination of SMAD3."
  val entities0 = Array("B-Protein", "O", "B-Protein", "I-Protein", "O", "O", "O", "O", "O", "B-Protein", "O")
  val sentence1 = "TGFBR2 binds to TGFBR1 and SMAD3."
  val entities1 = Array("B-Protein", "O", "O", "B-Protein", "O", "B-Protein", "O")
  // concatenate sentences
  val text = s"$sentence0 $sentence1"

  // rules are written in yaml and support yaml comments (start with the '#' character)
  val rules = """| # this rule creates Protein mentions from named entity tags in IOB notation
                 |- name: rule1
                 |  label: Protein
                 |  type: token
                 |  pattern: |
                 |    [entity='B-Protein'] [entity='I-Protein']*
                 |
                 | # this rule creates phosphorylation events
                 | # the arguments are Protein mentions
                 |- name: rule2
                 |  label: [Phosphorylation, Event]
                 |  pattern: |
                 |    Trigger = [word=/^phospho/ & tag=/^VB/] (?! of)
                 |    theme: Protein = dobj
                 |    cause: Protein = nsubj
                 |
                 | # creates ubiquitination events
                 | # note that the "trigger" field name is case insensitive
                 |- name: rule3
                 |  label:
                 |    - Ubiquitination
                 |    - Event
                 |  pattern: |
                 |    triggER = ubiquitination (?= of)
                 |    theme: Protein = prep_of
                 |
                 | # creates a regulation with a ubiquitination event as the theme
                 |- name: rule4
                 |  label: DownRegulation
                 |  pattern: |
                 |    trigger = [lemma=inhibit]
                 |    theme: Event = dobj
                 |    cause: Protein = nsubj
                 |
                 | # example of a surface rule that captures an event
                 | # note that you need to capture a trigger for this to be an event mention
                 | # this event doesn't make sense biologically, is just an example
                 |- name: rule5
                 |  label: XXXtestXXX
                 |  type: token
                 |  pattern: |
                 |    @theme:Phosphorylation and (?<TriGgEr> inhibits) the @cause:Ubiquitination
                 |
                 | # example of an argument with a quantifier
                 | # with the '+' this rule finds a binding with 3 themes
                 | # without the '+' this rule finds 3 bindings with 1 theme each
                 |- name: rule6
                 |  label: Binding
                 |  pattern: |
                 |    trigger = binds
                 |    theme: Protein+ = nsubj | prep_to
                 |
                 |- name: rule7
                 |  label: ExampleRelation
                 |  pattern: |
                 |    prot1: Protein
                 |    prot2: Protein = conj_and
                 |""".stripMargin

  // creates an extractor engine using the rules and the default actions
  val extractor = new ExtractorEngine(rules)

  // annotate the sentences and override the named entity tags
  val proc = new BioNLPProcessor
  val doc = proc annotate text
  doc.sentences(0).entities = Some(entities0)
  doc.sentences(1).entities = Some(entities1)

  // extract mentions from annotated document
  val mentions = extractor extractFrom doc

  // code used for printing the found mentions
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
