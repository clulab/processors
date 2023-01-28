package controllers

import org.clulab.processors.Document
import org.clulab.processors.Sentence

class ParseObj(doc: Document) {

  def mkParseObj(sentence: Sentence, sb: StringBuilder): Unit = {

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
          .append(getTd(edgesToString(i)))
          .append("</tr>")
    }
  }

  def mkHtml: String = {
    val header =
      """
        |  <tr>
        |    <th>Text</th>
        |    <th>Start</th>
        |    <th>End</th>
        |    <th>Word</th>
        |    <th>Tags</th>
        |    <th>Lemmas</th>
        |    <th>Entities</th>
        |    <th>Norms</th>
        |    <th>Chunks</th>
        |    <th>Dependencies</th>
        |  </tr>
      """.stripMargin
    val sb = new StringBuilder(header)

    doc.sentences.indices.foreach{ i =>
      val sentence = doc.sentences(i)

      sb.append(s"<tr><td colspan='10' align='center'>Sentence ${i + 1}, sentence.equivalenceHash = ${sentence.equivalenceHash}, dependencies.equivalenceHash = ${sentence.dependencies.get.equivalenceHash}</td></tr>")
      mkParseObj(sentence, sb)
    }
    sb.toString
  }
}
