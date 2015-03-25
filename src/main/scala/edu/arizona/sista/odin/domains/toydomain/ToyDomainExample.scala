package edu.arizona.sista.odin.domains.toydomain

import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.odin._

object ToyDomainExample extends App {
  // two example sentences
  val text = """|TGFBR2 phosphorylates peri-kappa B and inhibits the ubiquitination of SMAD3.
                |TGFBR2 binds to TGFBR1 and SMAD3.
                |""".stripMargin

  // rules are written in yaml and support yaml comments (starting with the '#' character)
  val rules = """| # this rule creates Protein mentions from named entity tags in IOB notation
                 |- name: rule1
                 |  label: Protein
                 |  type: token
                 |  pattern: |
                 |    [entity='B-GENE'] [entity='I-GENE']*
                 |
                 | # this rule creates phosphorylation events
                 | # the arguments are Protein mentions
                 |- name: rule2
                 |  label: [Phosphorylation, Event]
                 |  pattern: |
                 |    trigger = [word=/^phospho/ & tag=/^VB/]
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
                 |    trigger = ubiquitination
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
                 |- name: rule5
                 |  label: DownRegulation
                 |  type: token
                 |  pattern: |
                 |    @theme:Phosphorylation and? (?<trigger> inhibits) the @cause:Ubiquitination
                 |
                 | # example of an argument with a quantifier
                 | # with the '+' this rule finds a single binding event with 3 themes
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

  // annotate the sentences
  // note: in another domains, you might prefer an open-domain processor, such as CoreNLPProcessor
  val proc = new BioNLPProcessor
  val doc = proc.annotate(text)

  // extract mentions from the annotated document
  val mentions = extractor.extractFrom(doc)

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
