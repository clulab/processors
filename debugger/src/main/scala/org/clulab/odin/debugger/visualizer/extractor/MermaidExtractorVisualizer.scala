package org.clulab.odin.debugger.visualizer.extractor

import org.clulab.odin.debugger.visualization.{HtmlVisualization, Visualization}
import org.clulab.odin.debugger.visualizer.HtmlStyling
import org.clulab.odin.impl.{CrossSentenceExtractor, Extractor, GraphExtractor, TokenExtractor}
import scalatags.Text
import scalatags.Text.all._

class MermaidExtractorVisualizer() extends ExtractorVisualizer() with HtmlStyling {

  def visualizeCrossSentenceExtractor(crossSentenceExtractor: CrossSentenceExtractor): Text.TypedTag[String] = {
    ??? // visualizeCrossSentenceExtractor(0, crossSentenceExtractor)
  }

  def visualizeGraphExtractor(graphExtractor: GraphExtractor): Text.TypedTag[String] = {
    ??? // visualizeGraphExtractor(0, graphExtractor)
  }

  def visualizeTokenExtractor(tokenExtractor: TokenExtractor): Text.TypedTag[String] = {
    val textVisualizer = new TextExtractorVisualizer()
    val placeholder = raw("&nbsp;" * 2)
    val extractions = textVisualizer.extractTokenPattern(0, tokenExtractor.pattern).map { case (name, value) =>
      (s"pattern:$name", value)
    }

    def toRows(string: String, colCount: Int): Seq[Text.TypedTag[String]] = {
      val lines = string.lines.toArray
      val rows = lines.map { line =>
        val indent = line.takeWhile(_ == ' ')
        val rest = line.drop(indent.length)
        val fragment = frag(
          span(raw("&nbsp;" * indent.length)),
          span(rest)
        )

        tr(
          td(colspan := colCount)(
            fragment
          )
        )
      }

      rows
    }

    val top = textVisualizer.visualizeTokenExtractor(0, tokenExtractor)
    val topRows = toRows(top, 3)
    val botRows = extractions.flatMap { case (name, string) =>
      val headerRow = tr(
        td(placeholder),
        td(colspan := 2)(name)
      )
      val trailerRow = {
        val mermaid = pre(`class` := "mermaid")("""
          graph TD
          A[1 SaveStart --GLOBAL--] -. next .-> B[2 MatchToken B-PER]
          B -- next --> C[3 Split]
          C -. lhs .-> D[4 MatchToken I-PER]
          C -. rhs .-> E[5 Pass]
          D -- next --> C
          E -. next .-> F[6 SaveEnd --GLOBAL--]
          F -. next .-> G[0 Done]
        """)

        // need the val and where they go to
        // also figure out what kind of line, solid or dashed
        // also take look ahead and behind into account
        // somehow insert into pipeline with auto

        tr(
          td(placeholder),
          td(placeholder),
          td(
            mermaid
          )
        )
      }

      Seq(headerRow) :+ trailerRow
    }

    table(`class` := bordered)(
      topRows,
      botRows
    )
  }

  override def visualize(extractor: Extractor): HtmlVisualization = {

    val frag = extractor match {
      case tokenExtractor: TokenExtractor => visualizeTokenExtractor(tokenExtractor)
      case graphExtractor: GraphExtractor => visualizeGraphExtractor(graphExtractor)
      case crossSentenceExtractor: CrossSentenceExtractor => visualizeCrossSentenceExtractor(crossSentenceExtractor)
      case _ => ???
    }
    val visualization = new HtmlVisualization(frag)

    visualization
  }
}
