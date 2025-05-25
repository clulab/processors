package org.clulab.odin.debugger.visualizer.sentence

import org.clulab.odin.debugger.visualization.HtmlVisualization
import org.clulab.odin.debugger.visualizer.html.HtmlVisualizing
import org.clulab.processors.Sentence
import scalatags.Text.all._

class HtmlSentenceVisualizer extends SentenceVisualizer with HtmlVisualizing {

  def mkRows(sentence: Sentence): Seq[Fragment] = {

    def edgesToString(to: Int): String = {
      val edges = sentence.dependencies.get.incomingEdges(to)
      val string = edges.map { edge =>
        s"${sentence.words(edge._1)}\u291c${edge._2}\u2192${sentence.words(to)}"
      }.mkString(", ")

      string
    }

    def getOrEmpty(seqOpt: Option[Seq[String]], index: Int): String =
        seqOpt.map(_(index)).getOrElse("")

    val rows = sentence.words.indices.map { i =>
      tr(
        td(`class` := right)(i.toString),
        td(sentence.raw(i)),
        td(sentence.startOffsets(i).toString),
        td(sentence.endOffsets(i).toString),
        td(sentence.words(i)),
        td(getOrEmpty(sentence.tags, i)),
        td(getOrEmpty(sentence.lemmas, i)),
        td(getOrEmpty(sentence.entities, i)),
        td(getOrEmpty(sentence.norms, i)),
        td(getOrEmpty(sentence.chunks, i)),
        td(edgesToString(i))
      )
    }

    rows
  }

  override def visualize(sentence: Sentence): HtmlVisualization = {
    val headerFrag = tr(
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
    )
    val rows = mkRows(sentence)
    val tableFrag = table(`class` := bordered)(
      headerFrag,
      rows
    )

    new HtmlVisualization(tableFrag)
  }
}
