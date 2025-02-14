package org.clulab.odin.debugger.visualizer.extractor

import org.clulab.odin.debugger.visualization.HtmlVisualization
import org.clulab.odin.debugger.visualizer.HtmlVisualizing
import org.clulab.odin.impl.{CrossSentenceExtractor, Done, Extractor, GraphExtractor, GraphPattern, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, RelationGraphPattern, SaveEnd, SaveStart, Split, TokenExtractor, TokenPattern, TriggerMentionGraphPattern, TriggerPatternGraphPattern}
import scalatags.Text
import scalatags.Text.all._

import scala.annotation.tailrec

class MermaidExtractorVisualizer() extends ExtractorVisualizer() with HtmlVisualizing {

  def esc(string: String): String = string.replaceAll("\"", "#quot;")

  def visualizeCrossSentenceExtractor(crossSentenceExtractor: CrossSentenceExtractor): Text.TypedTag[String] = {
    val textVisualizer = new TextExtractorVisualizer()
    val placeholder = raw("&nbsp;" * 2)
    val anchorExtraction = ("anchorPattern:pattern:", crossSentenceExtractor.anchorPattern.pattern)
    val neighborExtraction = ("neighborPattern:pattern:", crossSentenceExtractor.neighborPattern.pattern)
    val extractions = Seq(anchorExtraction, neighborExtraction)
    val textVisualization = textVisualizer.visualizeCrossSentenceExtractor(0, crossSentenceExtractor)
    val lines = textVisualization.lines
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

  def getMainChildren(inst: Inst): List[InstChild] = {

    def mkNextChild(inst: Inst, wide: Boolean): InstChild =
      InstChild("next", inst.getNext, wide)

    val children = inst match {
      case Done => List.empty
      case inst: Pass => List(mkNextChild(inst, true))
      case inst: Split => List(
        InstChild("lhs", inst.lhs, true),
        InstChild("rhs", inst.rhs, true)
      )
      case inst: SaveStart => List(mkNextChild(inst, true))
      case inst: SaveEnd => List(mkNextChild(inst, true))
      case inst: MatchToken => List(mkNextChild(inst, false))
      case inst: MatchMention => List(mkNextChild(inst, false))
      case inst: MatchSentenceStart => List(mkNextChild(inst, true))
      case inst: MatchSentenceEnd => List(mkNextChild(inst, true))
      case inst: MatchLookAhead => List(
        mkNextChild(inst, true) // ,
        // InstChild("start", inst.start, true)
      )
      case inst: MatchLookBehind => List(
        mkNextChild(inst.start, true) // ,
        // InstChild("start", inst.start, true)
      )
    }

    children
  }

  def extractMainInst(start: Inst): List[Inst] = {

    @tailrec
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

  // TODO: Fix indents and line feeds
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
          visualizeStartInst(inst.start, depth + 1, Some(inst)),
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
            indent(s"""${mkNodeId(Done)} == "pop" ==> ${mkNodeId(parentOpt.get)}\n""", depth - 1)
          )
        }

    frag(
      topFrag,
      midFrags,
      botFrag
    )
  }

  def visualizeTokenPattern2(tokenPattern: TokenPattern): Text.TypedTag[String] = {
    val fragment = visualizeStartInst(tokenPattern.start, 0, None)

    pre(`class` := "mermaid")(
      fragment
    )
  }

   def visualizeTokenPattern(tokenPattern: TokenPattern): Text.TypedTag[String] = {
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
        val mermaid1 = visualizeTokenPattern(tokenExtractor.pattern)
        val mermaid = visualizeTokenPattern2(tokenExtractor.pattern)
//        val string = mermaid2.toString
//        println(string)

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
