package org.clulab.processors.webapp2.serialization

import org.clulab.processors.{Document, Sentence}
import scalatags.Text.all._
import scalatags.generic.Frag
import scalatags.text.Builder

class ParseObj(doc: Document) {
  type Fragment = Frag[Builder, String]

  def mkParseObj(sentence: Sentence): Fragment = {

    def getTd(text: String, right: Boolean = false): Fragment = {
      td(style := (if (right) "text-align: right" else ""))(
        text
      )
    }

    def getTdAtOptString(option: Option[Seq[String]], n: Int): Fragment = {
      val text =
          if (option.isEmpty) ""
          else option.get(n)

      getTd(text)
    }

    def getTdAtString(values: Seq[String], n: Int): Fragment = getTd(values(n))

    def getTdAtInt(values: Seq[Int], n: Int): Fragment = getTd(values(n).toString, true)

    def edgesToString(to: Int): String = {
      val edges = sentence.dependencies.map(_.incomingEdges(to)).getOrElse(Array.empty[(Int, String)]).toSeq

      edges.map(edge => sentence.words(edge._1) + "\u291c" + edge._2 + "\u2192" + sentence.words(to)).mkString(", ")
    }

    frag(
      sentence.words.indices.map { i =>
        tr(
          td(style := "text-align: right")(i),
          getTdAtString(sentence.raw, i),
          getTdAtInt(sentence.startOffsets, i),
          getTdAtInt(sentence.endOffsets, i),
          getTdAtString(sentence.words, i),
          getTdAtOptString(sentence.tags, i),
          getTdAtOptString(sentence.lemmas, i),
          getTdAtOptString(sentence.entities, i),
          getTdAtOptString(sentence.norms, i),
          getTdAtOptString(sentence.chunks, i),
          getTd(edgesToString(i))
        )
      }
    )
  }

  def mkHtml: String = {
    val rows = doc.sentences.indices.map{ i =>
      val sentence = doc.sentences(i)
      val dependenciesHash = sentence.dependencies.map(_.equivalenceHash.toString).getOrElse("N/A")

      frag(
        tr(
          td(colspan := 11, style := "text-align: center")(
              s"Sentence ${i + 1}, sentence.equivalenceHash = ${sentence.equivalenceHash}, dependencies.equivalenceHash = $dependenciesHash"
          )
        ),
        mkParseObj(sentence)
      )
    }

    val result = table(style := "margin-top: 0")(
      tr(
        th("Index"),
        th("Raw"),
        th("Start"),
        th("End"),
        th("Word"),
        th("Tags"),
        th("Lemmas"),
        th("Entities"),
        th("Norms"),
        th("Chunks"),
        th("Dependencies")
      ),
      rows
    ).toString

    result
  }
}
