package controllers

import org.clulab.odin.Mention
import org.clulab.processors.{Document, Sentence}

class MentionsObj(mentions: Seq[Mention]) {

  def mkParseObj(mention: Mention, sb: StringBuilder): Unit = {
    val sentence = mention.sentenceObj

    def getTd(text: String): String = "<td>" + xml.Utility.escape(text) + "</td>"

    def getTdAtOptString(option: Option[Array[String]], n: Int): String = {
      val text =
        if (option.isEmpty) ""
        else option.get(n)

      getTd(text)
    }

    def getTdAtString(values: Array[String], n: Int): String = getTd(values(n))

    def getTdAtInt(values: Array[Int], n: Int): String = getTd(values(n).toString)

    def edgesToString(to: Int): String = {
      val edges = sentence.dependencies.get.incomingEdges(to)

      edges.map(edge => sentence.words(edge._1) + "\u291c" + edge._2 + "\u2192" + sentence.words(to)).mkString(", ")
    }

    sentence.words.indices.foreach { i =>
      sb
          .append("<th></th>")
          .append("<tr>")
          .append(getTdAtString(sentence.raw, i))
          .append(getTdAtInt(sentence.startOffsets, i))
          .append(getTdAtInt(sentence.endOffsets, i))
          .append(getTdAtString(sentence.words, i))
          .append(getTdAtOptString(sentence.tags, i))
          .append(getTdAtOptString(sentence.lemmas, i))
          .append(getTdAtOptString(sentence.entities, i))
          .append(getTdAtOptString(sentence.norms, i))
          .append(getTdAtOptString(sentence.chunks, i))
          .append(getTdAtString(sentence.raw, i))
          .append(getTd(edgesToString(i)))
          .append("</tr>")
    }
  }

  def mkHtml: String = {
    val header =
      """
        |  <tr>
        |    <th>Field</th>
        |    <th>Value</th>
        |  </tr>
      """.stripMargin
    val sb = new StringBuilder(header)

    mentions.foreach{ mention =>
      mkParseObj(mention, sb)
    }
    sb.toString
  }
}
