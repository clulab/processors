package org.clulab.odin.debugger.visualizer.extractor

import org.clulab.odin.debugger.visualization.MermaidVisualization
import org.clulab.odin.debugger.visualizer.html.HtmlVisualizing
import org.clulab.odin.impl.{CrossSentenceExtractor, Done, Extractor, GraphExtractor, GraphPattern, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, RelationGraphPattern, SaveEnd, SaveStart, Split, TokenExtractor, TokenPattern, TriggerMentionGraphPattern, TriggerPatternGraphPattern}
import org.clulab.utils.StringUtils
import scalatags.Text.all._

class MermaidExtractorVisualizer() extends ExtractorVisualizer() with HtmlVisualizing {
  val textExtractorVisualizer = new TextExtractorVisualizer()

  def esc(string: String): String = string.replaceAll("\"", "#quot;")

  def visualizeCrossSentenceExtractor(crossSentenceExtractor: CrossSentenceExtractor): Fragment = {
    val textVisualizer = new TextExtractorVisualizer()
    val placeholder = raw("&nbsp;" * 2)
    val anchorExtraction = ("anchorPattern:pattern:", crossSentenceExtractor.anchorPattern.pattern)
    val neighborExtraction = ("neighborPattern:pattern:", crossSentenceExtractor.neighborPattern.pattern)
    val extractions = Seq(anchorExtraction, neighborExtraction)
    val textVisualization = textVisualizer.visualizeCrossSentenceExtractor(0, crossSentenceExtractor)
    val lines = textVisualization.linesIterator
    val top = lines.takeWhile(_ != "anchorPattern:pattern:")
    val topRow = toRow(top.toSeq, 3)
    val botRows = extractions.flatMap { case (name, tokenPattern) =>
      val headerRow = tr(
        td(placeholder),
        td(colspan := 2)(name)
      )
      val trailerRow = {
        val mermaid = visualizeTokenPattern(tokenPattern)

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
      topRow,
      botRows
    )
  }

  def visualizeGraphPattern(graphPattern: GraphPattern): Fragment = {
    graphPattern match {
      case graphPattern: TriggerPatternGraphPattern =>
        visualizeTokenPattern(graphPattern.trigger)
      case graphPattern: TriggerMentionGraphPattern => span()
      case graphPattern: RelationGraphPattern => span()
    }
  }

  def visualizeGraphExtractor(graphExtractor: GraphExtractor): Fragment = {
    val textVisualizer = new TextExtractorVisualizer()
    val placeholder = raw("&nbsp;" * 2)
    val extractions = textVisualizer.extractGraphPattern(0, graphExtractor.pattern).map { case (name, value) =>
      (s"pattern:$name", value)
    }

    val textVisualization = textVisualizer.visualizeGraphExtractor(0, graphExtractor)
    val lines = textVisualization.linesIterator
    val top = lines.takeWhile(_ != "pattern:trigger:")
    val topRow = toRow(top.toSeq, 3)
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
      topRow,
      botRows
    )
  }

  def getShortDescription(inst: Inst): String = {
    val longDescription = textExtractorVisualizer.getDescription(0, inst)
    val separator = '('
    val shortDescription =
        if (longDescription.contains(separator)) {
          val name = StringUtils.beforeFirst(longDescription, separator)
          val description = StringUtils.afterFirst(longDescription, separator).dropRight(1)

          s"$name\n$description"
        }
        else longDescription

    shortDescription
  }

  def getMainChildren(inst: Inst): List[InstChild] = {
    val children = textExtractorVisualizer.getChildren(inst)
    val mainChildren = children.filter { case InstChild(name, _, _) =>
      name != "start"
    }

    mainChildren
  }

  def extractMainInst(start: Inst): List[Inst] = {

    @annotation.tailrec
    def loop(todos: List[Inst], visiteds: Set[Inst], dones: List[Inst]): List[Inst] = {
      todos match {
        case Nil => dones.reverse
        case head :: tail =>
          if (visiteds(head)) loop(tail, visiteds, dones)
          else loop(getMainChildren(head).map(_.inst) ++ tail, visiteds + head, head :: dones)
      }
    }

    val unsortedInsts = loop(List(start), Set.empty, List.empty)

    unsortedInsts
  }

  def visualizeStartInst(start: Inst, depth: Int, parentOpt: Option[Inst]): Fragment = {
    assert(if (depth == 0) parentOpt.isEmpty else parentOpt.isDefined)

    val isGraph = parentOpt.isEmpty

    def indent(string: String, depth: Int = depth): String = {
      val indentation = "  " * depth

      s"$indentation$string"
    }

    def mkNodeId(inst: Inst): String = {
      val posId = inst.getPosId

      if (posId == 0  && !isGraph) {
        assert(inst == Done)
        s"N${parentOpt.get.getPosId}D"
      }
      else
        s"N$posId"
    }

    val topFrag =
        if (isGraph) frag("\ngraph TD\n")
        else frag(indent(s"""subgraph "${parentOpt.get.getPosId} ${getShortDescription(parentOpt.get)}"\n""", depth - 1))
    val mainChildren = extractMainInst(start)
    val midFrags = mainChildren.map { inst =>
      val node = {
        val posId = inst.getPosId
        val nodeId = mkNodeId(inst)
        val description = getShortDescription(inst)
        val label = esc(s"$posId $description")

        frag(indent(s"""$nodeId["$label"]\n"""))
      }
      val instChildren = getMainChildren(inst)
      val mainEdges = instChildren.map { case InstChild(name, child, wide) =>
        val label = inst match {
          case _: MatchLookAhead => esc(s"2 $name")
          case _: MatchLookBehind => esc(s"2 $name")
          case _: Inst => esc(name)
        }

        if (wide) frag(indent(s"""N${inst.getPosId} == "$label" ==> ${mkNodeId(child)}\n"""))
        else frag(indent(s"""N${inst.getPosId} -- "$label" --> ${mkNodeId(child)}\n"""))
      }
      val sideEdges = inst match {
        case inst: MatchLookAhead => List(
          frag(indent(s"""N${inst.getPosId} == "1 start" ==> N${inst.start.getPosId}\n""")),
          visualizeStartInst(inst.start, depth + 1, Some(inst))
        )
        case inst: MatchLookBehind => List(
          frag(indent(s"""N${inst.getPosId} == "1 start" ==> N${inst.start.getPosId}\n""")),
          visualizeStartInst(inst.start, depth + 1, Some(inst))
        )
        case _: Inst => List.empty
      }

      frag(
        node,
        sideEdges,
        mainEdges
      )
    }
    val botFrag =
        if (isGraph) frag()
        else {
          frag(
            indent("end\n", depth - 1),
            indent(s"""${mkNodeId(Done)} == "return" ==> ${mkNodeId(parentOpt.get)}\n""", depth - 1)
          )
        }

    frag(
      topFrag,
      midFrags,
      botFrag
    )
  }

  def visualizeTokenPattern(tokenPattern: TokenPattern): Fragment = {
    val fragment = visualizeStartInst(tokenPattern.start, 0, None)

    pre(`class` := "mermaid")(
      fragment
    )
  }

  def visualizeTokenExtractor(tokenExtractor: TokenExtractor): Fragment = {
    val textVisualizer = new TextExtractorVisualizer()
    val placeholder = raw("&nbsp;" * 2)
    val extractions = textVisualizer.extractTokenPattern(0, tokenExtractor.pattern).map { case (name, value) =>
      (s"pattern:$name", value)
    }
    val top = textVisualizer.visualizeTokenExtractor(0, tokenExtractor)
    val topRows = toRows(top.linesIterator.toSeq, 3)
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
          td(`class` := wide)(
            mermaid
          )
        )
      }

      Seq(headerRow) :+ trailerRow
    }

    table(`class` := s"$bordered $wide")(
      topRows,
      botRows
    )
  }

  override def visualize(extractor: Extractor): MermaidVisualization = {

    val frag = extractor match {
      case tokenExtractor: TokenExtractor => visualizeTokenExtractor(tokenExtractor)
      case graphExtractor: GraphExtractor => visualizeGraphExtractor(graphExtractor)
      case crossSentenceExtractor: CrossSentenceExtractor => visualizeCrossSentenceExtractor(crossSentenceExtractor)
      case _ => throw new RuntimeException(s"Unrecognized extractor: ${extractor.toString}")
    }
    val visualization = new MermaidVisualization(frag)

    visualization
  }
}
