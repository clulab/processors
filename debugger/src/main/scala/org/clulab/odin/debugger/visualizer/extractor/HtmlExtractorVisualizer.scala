package org.clulab.odin.debugger.visualizer.extractor

import org.clulab.odin.debugger.visualization.HtmlVisualization
import org.clulab.odin.impl.{CrossSentenceExtractor, Extractor, GraphExtractor, TokenExtractor}
import scalatags.Text
import scalatags.Text.all._

class HtmlExtractorVisualizer extends ExtractorVisualizer {

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
    val topRows = toRows(top, 4)
    val botRows = extractions.flatMap { case (name, string) =>
      val headerRow = tr(
        td(placeholder),
        td(colspan := 3)(name)
      )
      val trailerRows = string.lines.map { line =>
        val number = line.takeWhile(_ != '.')
        val indent = line
            .drop(number.length + 2) // Skip . and first space.
            .takeWhile(_ == ' ')
        val rest = line.drop(number.length + 2 + indent.length)

        tr(
          td(placeholder),
          td(placeholder),
          td(number),
          td(
            raw("&nbsp;" * indent.length),
            rest
          )
        )
      }

      Seq(headerRow) ++ trailerRows
    }

    table(`class` := "bordered")(
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
