package org.clulab.odin.debugger.visualizer.extractor

import org.clulab.odin.debugger.visualization.{HtmlVisualization, Visualization}
import org.clulab.odin.debugger.visualizer.HtmlStyling
import org.clulab.odin.impl.{CrossSentenceExtractor, Done, Extractor, GraphExtractor, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, SaveEnd, SaveStart, Split, TokenExtractor, TokenPattern}
import scalatags.Text
import scalatags.Text.all._

import scala.annotation.tailrec

class MermaidExtractorVisualizer() extends ExtractorVisualizer() with HtmlStyling {

  def visualizeCrossSentenceExtractor(crossSentenceExtractor: CrossSentenceExtractor): Text.TypedTag[String] = {
    ??? // visualizeCrossSentenceExtractor(0, crossSentenceExtractor)
  }

  def visualizeGraphExtractor(graphExtractor: GraphExtractor): Text.TypedTag[String] = {
    //??? // visualizeGraphExtractor(0, graphExtractor)
    p("Keith was here")
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
      case inst: MatchLookAhead => s"negative = ${inst.negative}"
      case inst: MatchLookBehind => s"negative = ${inst.negative}"
    }
    val formattedDetails =
      if (details.isEmpty) ""
      else s"($details)"

    s"$name$formattedDetails"
  }

  def visualizeTokenPattern(tokenPattern: TokenPattern): Text.TypedTag[String] = {
    val start = tokenPattern.start
    val insts = extractInst(start)
    val nodes = insts.map { inst =>
      val posId = inst.getPosId
      val description = getShortDescription(inst)

      s"N$posId[$posId $description]\n"
    }
    val edges = insts.flatMap { parent =>
      val namedChildren = getChildren(parent)
      val edges = namedChildren.map { case InstChild(name, child, wide) =>
        // If it goes to done and it is from a start, then complications.
        // Maybe add N#start and N#done as nodes?
        if (wide) s"N${parent.getPosId} == $name ==> N${child.getPosId}\n"
        else s"N${parent.getPosId} -- $name --> N${child.getPosId}\n"
      }

      edges
    }
    val rawEdges = edges.map(raw)

    pre(`class` := "mermaid")(
      "\ngraph TD\n",
      nodes,
      rawEdges
    )
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
    val botRows = extractions.flatMap { case (name, _) =>
      val headerRow = tr(
        td(placeholder),
        td(colspan := 2)(name)
      )
      val trailerRow = {
        val mermaid2 = pre(`class` := "mermaid")("""
          graph TD
          N1[1 SaveStart --GLOBAL--] -. next .-> N2[2 MatchToken B-PER]
          N2 -- next --> N3[3 Split]
          N3 -. lhs .-> N4[4 MatchToken I-PER]
          N3 -. rhs .-> N5[5 Pass]
          N4 -- next --> N3
          N5 -. next .-> N6[6 SaveEnd --GLOBAL--]
          N6 -. next .-> N7[0 Done]
        """)
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
