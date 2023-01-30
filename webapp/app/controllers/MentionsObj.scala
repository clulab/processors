package controllers

import org.clulab.odin.{CrossSentenceMention, EventMention, Mention, RelationMention, TextBoundMention}

class MentionsObj(mentions: Seq[Mention]) {
  val tableHeader = """
    |<table style="margin-top: 0;">
    |""".stripMargin
  val tableTrailer = """
    |</table>
    |""".stripMargin
  val leftTdHeader = """
    |<tr>
    |  <td align="right">
    |""".stripMargin
  val rightTdHeader = """
    |<tr>
    |  <td>
    |""".stripMargin
  val tdSeparator = """
    |  </td>
    |  <td>
    |""".stripMargin
  val tdTrailer = """
    |  </td>
    |</tr>
    |""".stripMargin

  def getTrSeparator(wide: Boolean): String = {
    val style = if (wide) """ style = "width: 100%;"""" else ""
    s"""
      |<tr>
      |  <th>Field</th>
      |  <th$style>Value</th>
      |</tr>
      |""".stripMargin
  }

  def getTd(field: String, text: String): String =
    s"""
       |$leftTdHeader
       |    ${xml.Utility.escape(field)}:&nbsp;
       |$tdSeparator
       |    ${xml.Utility.escape(text)}
       |$tdTrailer
       |""".stripMargin

  def getTds(field: String, strings: Seq[String]): String =
      getTd(field, strings.mkString(", "))

  def openTable(field: String): String = s"""
      |$leftTdHeader
      |  ${xml.Utility.escape(field)}:&nbsp;
      |$tdSeparator
      |  $tableHeader
      |""".stripMargin

  val closeTable: String = s"""
      |  $tableTrailer
      |$tdTrailer
      |""".stripMargin

  def mkMentionsObj(mention: Mention, sb: StringBuilder, nameOpt: Option[String] = None, depth: Int = 0): Unit = {
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

    sb
        .append(getTrSeparator(depth != 0))
        .append(getTd ("Sentence #", (mention.sentence + 1).toString))
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
        sb.append(openTable("Trigger"))
        mkMentionsObj(eventMention.trigger, sb, None, depth + 1)
        sb.append(closeTable)
      case relationMention: RelationMention =>
      case crossSentenceMention: CrossSentenceMention =>
      case _ =>
    }

    if (mention.arguments.nonEmpty) {
      sb.append(openTable("Arguments"))
      for (name <- mention.arguments.keys.toSeq.sorted; mention <- mention.arguments(name).sorted)
        mkMentionsObj(mention, sb, Some(name), depth + 1)
      sb.append(closeTable)
    }
  }

  def mkHtml: String = {
    val sb = new StringBuilder(tableHeader)

    mentions.foreach { mention =>
      mkMentionsObj(mention, sb)
    }
    sb.append(tableTrailer)
    sb.toString
  }
}
