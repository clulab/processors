package org.clulab.odin.debugger.visualizer.extractor

import org.clulab.odin.debugger.visualization.{HtmlVisualization, Visualization}
import org.clulab.odin.debugger.visualizer.{HtmlStyling, HtmlVisualizer}
import org.clulab.odin.impl.{CrossSentenceExtractor, Done, Extractor, GraphExtractor, GraphPattern, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, RelationGraphPattern, SaveEnd, SaveStart, Split, TokenExtractor, TokenPattern, TriggerMentionGraphPattern, TriggerPatternGraphPattern}
import scalatags.Text
import scalatags.Text.all._

import scala.annotation.tailrec

class MermaidExtractorVisualizer() extends ExtractorVisualizer() with HtmlVisualizer {

  def visualizeCrossSentenceExtractor(crossSentenceExtractor: CrossSentenceExtractor): Text.TypedTag[String] = {
    p("Keith was here") // ??? // visualizeCrossSentenceExtractor(0, crossSentenceExtractor)
  }

  def visualizeGraphPattern(graphPattern: GraphPattern): Text.TypedTag[String] = {
    graphPattern match {
      case graphPattern: TriggerPatternGraphPattern =>
        visualizeTokenPattern(graphPattern.trigger)
      case graphPattern: TriggerMentionGraphPattern => span()
      case graphPattern: RelationGraphPattern => span()
    }
  }

  def visualizeGraphExtractor(graphExtractor: GraphExtractor): Text.TypedTag[String] = {
    val textVisualizer = new TextExtractorVisualizer()
    val placeholder = raw("&nbsp;" * 2)
    val extractions = textVisualizer.extractGraphPattern(0, graphExtractor.pattern).map { case (name, value) =>
      (s"pattern:$name", value)
    }

    val textVisualization = textVisualizer.visualizeGraphExtractor(0, graphExtractor)
    val lines = textVisualization.lines
    val top = lines.takeWhile(_ != "pattern:trigger:")
    val topRows = toRows(top.toSeq, 3)
    val botRows = extractions.flatMap { case (name, _) =>
      val headerRow = tr(
        td(placeholder),
        td(colspan := 2)(name)
      )
      val trailerRow = {
        val mermaid = visualizeGraphPattern(graphExtractor.pattern)

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

  def getShortDescription(inst: Inst): String = {
    val name = inst.getClass.getSimpleName
    val stringEmpty = ""
    val details = inst match {
      case Done => stringEmpty
      case inst: Pass => stringEmpty
      case inst: Split => stringEmpty
      case inst: SaveStart => stringEmpty // s"name = ${inst.name}"
      case inst: SaveEnd => stringEmpty // s"name = ${inst.name}"
      case inst: MatchToken => stringEmpty // s"c = ${visualizeTokenConstraint(indent, inst.c)}"
      case inst: MatchMention => stringEmpty // s"m = ${visualizeStringMatcher(indent, inst.m)}, name = ${inst.name}, arg = ${inst.arg}"
      case inst: MatchSentenceStart => stringEmpty
      case inst: MatchSentenceEnd => stringEmpty
      case inst: MatchLookAhead => stringEmpty // s"negative = ${inst.negative}"
      case inst: MatchLookBehind => stringEmpty // s"negative = ${inst.negative}"
    }
    val formattedDetails =
      if (details.isEmpty) ""
      else s"($details)"

    s"$name$formattedDetails"
  }

  def visualizeTokenPattern(tokenPattern: TokenPattern): Text.TypedTag[String] = {

    def esc(string: String): String = string.replaceAll("\"", "#quot;")

    val start = tokenPattern.start
    val insts = extractInst(start)
    val nodes = insts.map { inst =>
      val posId = inst.getPosId
      val description = getShortDescription(inst)
      val label = esc(s"$posId $description")

      s"""N$posId["$label"]\n"""
    }
    val rawNodes = nodes.map(raw)
    val edges = insts.flatMap { parent =>
      val namedChildren = getChildren(parent)
      val edges = namedChildren.map { case InstChild(name, child, wide) =>
        val label = esc(name)
        // If it goes to done and it is from a start, then complications.
        // Maybe add N#start and N#done as nodes?
        if (wide) s"""N${parent.getPosId} == "$label" ==> N${child.getPosId}\n"""
        else s"""N${parent.getPosId} -- "$label" --> N${child.getPosId}\n"""
      }

      edges
    }
    val rawEdges = edges.map(raw)

    pre(`class` := "mermaid")(
      "\ngraph TD\n",
      rawNodes,
      rawEdges
    )
  }

  def visualizeTokenExtractor(tokenExtractor: TokenExtractor): Text.TypedTag[String] = {
    val textVisualizer = new TextExtractorVisualizer()
    val placeholder = raw("&nbsp;" * 2)
    val extractions = textVisualizer.extractTokenPattern(0, tokenExtractor.pattern).map { case (name, value) =>
      (s"pattern:$name", value)
    }
    val top = textVisualizer.visualizeTokenExtractor(0, tokenExtractor)
    val topRows = toRows(top.lines.toSeq, 3)
    val botRows = extractions.flatMap { case (name, _) =>
      val headerRow = tr(
        td(placeholder),
        td(colspan := 2)(name)
      )
      val trailerRow = {
        val mermaid = visualizeTokenPattern(tokenExtractor.pattern)

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
