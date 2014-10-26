package edu.arizona.sista.matcher

import edu.arizona.sista.processors.{Document, Sentence}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

object actions {
  def mkPhosphorylation(doc: Document, sent: Int, state: State, found: Map[String, Seq[Int]]): Seq[Mention] = {
    val trigger = new TriggerMention("Phosphorylation trigger", sent, Interval(found("trigger").head))

    val themes = found("theme") flatMap { tok =>
      state.mentionsFor(sent, tok) filter {
        case mention: EntityMention => mention.label == "Protein"
        case _ => false
      }
    }

    val causes = found("cause") flatMap { tok =>
      state.mentionsFor(sent, tok) filter {
        case mention: EntityMention => mention.label == "Protein"
        case _ => false
      }
    }

    for {
      theme <- themes
      cause <- causes
    } yield new EventMention("Phosphorylation event", sent, Map("trigger" -> trigger, "theme" -> theme, "cause" -> cause))

  }

  def mkUbiquitination(doc: Document, sent: Int, state: State, found: Map[String, Seq[Int]]): Seq[Mention] = {
    val trigger = new TriggerMention("Ubiquitination trigger", sent, Interval(found("trigger").head))

    val themes = found("theme") flatMap { tok =>
      state.mentionsFor(sent, tok) filter {
        case mention: EntityMention => mention.label == "Protein"
        case _ => false
      }
    }

    for (theme <- themes) yield new EventMention("Ubiquitination event", sent, Map("trigger" -> trigger, "theme" -> theme))
  }

  def mkDownRegulation(doc: Document, sent: Int, state: State, found: Map[String, Seq[Int]]): Seq[Mention] = {
    val trigger = new TriggerMention("DownRegulation trigger", sent, Interval(found("trigger").head))

    val themes = found("theme") flatMap { tok =>
      state.mentionsFor(sent, tok) filter {
        case mention: EventMention => true
        case _ => false
      }
    }

    val causes = found("cause") flatMap { tok =>
      state.mentionsFor(sent, tok) filter {
        case mention: EntityMention => mention.label == "Protein"
        case _ => false
      }
    }

    for {
      theme <- themes
      cause <- causes
    } yield new EventMention("DownRegulation event", sent, Map("trigger" -> trigger, "theme" -> theme, "cause" -> cause))
  }
}

object TestEngine extends App {
  val text = "TGFBR2 phosphorylates peri-kappa B and inhibits the ubiquitination of SMAD3."
  val entities = Array("Protein", "O", "Protein", "Protein", "O", "O", "O", "O", "O", "Protein", "O")

  val rules = """
    name: phosphorylation
    priority: 1
    action: mkPhosphorylation
    pattern: {{
      trigger: [word=/^phospho/ & tag=/^VB/]
      theme: dobj
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
      theme: dobj
      cause: nsubj
    }}
  """

  def words(sentence: Sentence, interval: Interval) =
    sentence.words.slice(interval.start, interval.end).mkString(" ")

  val extractor = new ExtractorEngine(rules, actions)

  val processor = new CoreNLPProcessor

  val doc = processor annotate text
  doc.sentences.head.entities = Some(entities)

  val mentions = extractor extractFrom doc

  println
  println
  println(s"sentence = '$text'")
  println
  println("found:")
  println
  for (m <- mentions) {
    m match {
      case mention: EventMention =>
        val sent = mention.sentence

        println(mention.label)

        println(s"string = '${words(doc.sentences(sent), m.tokenInterval)}'")

        mention.arguments foreach {
          case (k,v) => println(s"$k = '${words(doc.sentences(sent), v.tokenInterval)}'")
        }

        println("=" * 72)

      case _ => Unit
    }
  }
}
