package controllers

import org.clulab.odin.{CrossSentenceMention, EventMention, Mention, RelationMention, TextBoundMention}
import org.clulab.processors.{Document, Sentence}

class MentionsObj(mentions: Seq[Mention]) {

  def mkMentionsObj(mention: Mention, sb: StringBuilder, nameOpt: Option[String] = None, depth: Int = 0): Unit = {
    val sentence = mention.sentenceObj
    val tokenInterval = mention.tokenInterval
    val indent = "&nbsp;&nbsp;&nbsp;&nbsp;" * depth
    val name = nameOpt.getOrElse("<none>")
    val labels = mention.labels
    val words = tokenInterval.map(sentence.words)
    val tags = sentence.tags.map(tokenInterval.map(_)).getOrElse(Seq.empty)
    val lemmas = sentence.lemmas.map(tokenInterval.map(_)).getOrElse(Seq.empty)
    val entities = sentence.entities.map(tokenInterval.map(_)).getOrElse(Seq.empty)
    val norms = sentence.norms.map(tokenInterval.map(_)).getOrElse(Seq.empty)
    val chunks = sentence.chunks.map(tokenInterval.map(_)).getOrElse(Seq.empty)
    val raws = tokenInterval.map(sentence.raw)

    def getTd(field: String, text: String): String = s"""
      |<tr>
      |  <td align="right">
      |    ${xml.Utility.escape(field)}:&nbsp;
      |  </td>
      |  <td>
      |    $indent${xml.Utility.escape(text)}
      |  </td>
      |</tr>
      |""".stripMargin

    def getTds(field: String, strings: Seq[String]): String =
        getTd(field, strings.mkString(", "))

    sb
        .append(getTd ("Name", name))
        .append(getTd ("Type", mention.getClass.getSimpleName))
        .append(getTd ("FoundBy", mention.foundBy))
        .append(getTd ("Sentence", mention.sentenceObj.getSentenceText))
        .append(getTds("Labels", labels))
        .append(getTds("Words", words))
        .append(getTds("Tags", tags))
        .append(getTds("Lemmas", lemmas))
        .append(getTds("Entities", entities))
        .append(getTds("Norms", norms))
        .append(getTds("Chunks", chunks))
        .append(getTds("Raw", raws))
        .append(getTds("Attachments", mention.attachments.toSeq.map(_.toString).sorted))

    mention match {
      case textBoundMention: TextBoundMention =>
      case eventMention: EventMention =>
        sb.append(getTd("Trigger", ""))
        mkMentionsObj(eventMention.trigger, sb, None, depth + 1)
      case relationMention: RelationMention =>
      case crossSentenceMention: CrossSentenceMention =>
      case _ =>
    }

    if (mention.arguments.nonEmpty) {
      sb.append(getTd("Arguments", ""))
      for (name <- mention.arguments.keys.toSeq.sorted; mention <- mention.arguments(name).sorted)
        mkMentionsObj(mention, sb, Some(name), depth + 1)
    }
    sb.append("<th></th>")
  }

  def mkHtml: String = {
    val header = """
      |  <tr>
      |    <th>Field</th>
      |    <th>Value</th>
      |  </tr>
      |""".stripMargin
    val sb = new StringBuilder(header)

    mentions.foreach{ mention =>
      mkMentionsObj(mention, sb)
    }
    sb.toString
  }
}
