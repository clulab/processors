package org.clulab.processors.webapp2.serialization

import org.clulab.odin._
import scalatags.Text.all._
import scalatags.generic.Frag
import scalatags.text.Builder

class MentionsObj(mentions: Seq[Mention]) {
  type Fragment = Frag[Builder, String]

  def getTrSeparator(wide: Boolean): Fragment = {
    tr(
      th("Field"),
      th(style := (if (wide) "width: 100%;" else ""))("Value")
    )
  }

  def getTrField(field: String, text: String): Fragment = {
    tr(
      td(style := "text-align: right")(s"$field:" ),
      td(text)
    )
  }

  def getTrFields(field: String, strings: Seq[String]): Fragment =
      getTrField(field, strings.mkString(", "))

  def getTrTable(field: String, fragment: Fragment): Fragment = {
    tr(
      td(style := "text-align: right")(
        s"$field:",
        raw("&nbsp;")
      ),
      td(
        table(style := "margin-top: 0;")(
          fragment
        )
      )
    )
  }

  def mkMentionsObj(mention: Mention, nameOpt: Option[String] = None, depth: Int = 0): Fragment = {
    val sentence = mention.sentenceObj
    val tokenInterval = mention.tokenInterval
    val name = nameOpt.getOrElse("<none>")
    val labels = mention.labels
    val words = tokenInterval.map(sentence.words)
    val tags = sentence.tags.map(tokenInterval.map(_)).getOrElse(Seq.empty)
    val lemmas = sentence.lemmas.map(tokenInterval.map(_)).getOrElse(Seq.empty)
    val entities = sentence.entities.map(tokenInterval.map(_)).getOrElse(Seq.empty)
    val norms = sentence.norms.map(tokenInterval.map(_)).getOrElse(Seq.empty)
    val chunks = sentence.chunks.map(tokenInterval.map(_)).getOrElse(Seq.empty)
    val raws = tokenInterval.map(sentence.raw)

    val sentenceRows = frag(
      getTrSeparator(depth != 0),
      getTrField ("Sentence #", (mention.sentence + 1).toString),
      getTrField ("Name", name),
      getTrField ("Type", mention.getClass.getSimpleName),
      getTrField ("FoundBy", mention.foundBy),
      getTrField ("Sentence", mention.sentenceObj.getSentenceText),
      getTrFields("Labels", labels),
      getTrFields("Words", words),
      getTrFields("Tags", tags),
      getTrFields("Lemmas", lemmas),
      getTrFields("Entities", entities),
      getTrFields("Norms", norms),
      getTrFields("Chunks", chunks),
      getTrFields("Raw", raws),
      getTrFields("Attachments", mention.attachments.toSeq.map(_.toString).sorted)
    )

    val mentionRows = mention match {
      case textBoundMention: TextBoundMention => frag()
      case eventMention: EventMention =>
        getTrTable("Trigger", mkMentionsObj(eventMention.trigger, None, depth + 1))
      case relationMention: RelationMention => frag()
      case crossSentenceMention: CrossSentenceMention => frag()
      case _ => frag()
    }

    val argumentRows = if (mention.arguments.nonEmpty) {
      val mentionsObjs = mention.arguments.keys.toSeq.sorted.flatMap { name =>
        mention.arguments(name).sorted.map { mention =>
          mkMentionsObj(mention, Some(name), depth + 1)
        }
      }

      getTrTable("Arguments", mentionsObjs)
    }
    else frag()

    frag(
      sentenceRows,
      mentionRows,
      argumentRows
    )
  }

  def mkHtml: String = {
    val mentionsObjs = mentions.map { mention =>
      mkMentionsObj(mention)
    }
    val fragment = table(style := "margin-top: 0;")(
      mentionsObjs
    )

    fragment.toString
  }
}
