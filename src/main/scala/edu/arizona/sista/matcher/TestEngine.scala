package edu.arizona.sista.matcher

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.{Document, Sentence}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

object actions {
  def mkPhosphorylation(sent: Int, state: State, found: Map[String, Seq[Int]]): Seq[Mention] = {
    val trigger = new TriggerMention("trigger", sent, Interval(found("trigger").head))

    val themes = state.mentionsFor(sent, found("theme"), "Protein")
    val causes = state.mentionsFor(sent, found("cause"), "Protein")

    for {
      theme <- themes
      cause <- causes
    } yield new EventMention("Phosphorylation", trigger, Map("theme" -> Seq(theme), "cause" -> Seq(cause)))

  }

  def mkUbiquitination(sent: Int, state: State, found: Map[String, Seq[Int]]): Seq[Mention] = {
    val trigger = new TriggerMention("trigger", sent, Interval(found("trigger").head))

    val themes = state.mentionsFor(sent, found("theme"), "Protein")

    for (theme <- themes) yield new EventMention("Ubiquitination", trigger, Map("theme" -> Seq(theme)))
  }

  def mkDownRegulation(sent: Int, state: State, found: Map[String, Seq[Int]]): Seq[Mention] = {
    val trigger = new TriggerMention("trigger", sent, Interval(found("trigger").head))

    val causes = state.mentionsFor(sent, found("cause"), "Protein")
    val themes = found("theme") flatMap { tok =>
      state.mentionsFor(sent, tok) filter {
        case mention: EventMention => true
        case _ => false
      }
    }

    for {
      theme <- themes
      cause <- causes
    } yield new EventMention("DownRegulation", trigger, Map("theme" -> Seq(theme), "cause" -> Seq(cause)))
  }
}

object TestEngine extends App {
  val text = "TGFBR2 phosphorylates peri-kappa B and inhibits the ubiquitination of SMAD3."
  val entities = Array("Protein", "O", "Protein", "Protein", "O", "O", "O", "O", "O", "Protein", "O")

  val rules = """
    name: phosphorylation
    priority: 1 # This is a comment!
    action: mkPhosphorylation
    pattern: {{
		  # this comment line should go away...
      trigger: [word=/^phospho/ & tag=/^VB/] # Here's another comment!
      # this one should go away too
      theme: dobj [mention=Protein]
      cause: nsubj
    }}

    name: ubiquitination
    priority: 1
    action: mkUbiquitination
    pattern: {{
      trigger: [word=ubiquitination]
      theme: prep_of
    }}

    name: down_regulation
    priority: 2
    action: mkDownRegulation
    pattern: {{
      trigger: [lemma=inhibit]
      theme: dobj [mention=/^Ubi/ & !mention=Phosphorylation]
      cause: nsubj
    }}
  """

  def words(sentence: Sentence, interval: Interval) =
    sentence.words.slice(interval.start, interval.end).mkString(" ")

  val extractor = new ExtractorEngine(rules, actions, withIOB=false)

  val processor = new CoreNLPProcessor

  val doc = processor annotate text
  doc.sentences.head.entities = Some(entities)

  val mentions = extractor extractFrom doc

  println(s"\n\nsentence = '$text'")
  println("\nfound:\n")

  for (m <- mentions) {
    m match {
      case mention: EventMention =>
        val sent = mention.sentence

        println(mention.label)

        println(s"string = '${words(doc.sentences(sent), m.tokenInterval)}'")

        println(s"trigger = ${words(doc.sentences(sent), mention.trigger.tokenInterval)}")

        mention.arguments foreach {
          case (k,vs) => for (v <- vs) println(s"$k = '${words(doc.sentences(sent), v.tokenInterval)}'")
        }

        println("=" * 72)

      case _ => Unit
    }
  }
}
