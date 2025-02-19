package org.clulab.odin.debugger.visualizer.extractor

import org.clulab.odin.debugger.visualization.{TextVisualization, Visualization}
import org.clulab.odin.impl._
import org.clulab.utils.StringUtils

import java.io.PrintWriter
import scala.annotation.tailrec

class TextExtractorVisualizer() extends ExtractorVisualizer() {

  def pwPrintln(printWriter: PrintWriter, indent: Int, string: String): Unit = {
    val spaces = "  " * indent
    val spacedString = string.replaceAll("\n", "\n" + spaces)

    printWriter.println(s"$spaces$spacedString")
  }

  def visualizeEmbeddingsResource(indent: Int, embeddingsResource: EmbeddingsResource): String = {
    val className = embeddingsResource.getClass.getSimpleName
    val details = s"path = ${embeddingsResource.path}"
    val formattedDetails =
      if (details.isEmpty) ""
      else s"($details)"

    s"$className$formattedDetails"
  }

  def visualizeNumericExpression(indent: Int, numericExpression: NumericExpression): String = {
    val className = numericExpression.getClass.getSimpleName
    val details = numericExpression match {
      case numericExpression: Constant => s"value = ${numericExpression.value}"
      case numericExpression: Addition => s"lhs = ${visualizeNumericExpression(indent, numericExpression.lhs)}, rhs = ${visualizeNumericExpression(indent, numericExpression.rhs)}"
      case numericExpression: Subtraction => s"lhs = ${visualizeNumericExpression(indent, numericExpression.lhs)}, rhs = ${visualizeNumericExpression(indent, numericExpression.rhs)}"
      case numericExpression: Multiplication => s"lhs = ${visualizeNumericExpression(indent, numericExpression.lhs)}, rhs = ${visualizeNumericExpression(indent, numericExpression.rhs)}"
      case numericExpression: Division => s"lhs = ${visualizeNumericExpression(indent, numericExpression.lhs)}, rhs = ${visualizeNumericExpression(indent, numericExpression.rhs)}"
      case numericExpression: EuclideanQuotient => s"lhs = ${visualizeNumericExpression(indent, numericExpression.lhs)}, rhs = ${visualizeNumericExpression(indent, numericExpression.rhs)}"
      case numericExpression: EuclideanRemainder => s"lhs = ${visualizeNumericExpression(indent, numericExpression.lhs)}, rhs = ${visualizeNumericExpression(indent, numericExpression.rhs)}"
      case numericExpression: NegativeExpression => s"expr = ${visualizeNumericExpression(indent, numericExpression.expr)}"
      case numericExpression: SimilarityConstraint => s"w1 = ${numericExpression.w1}, embeddings = ${visualizeEmbeddingsResource(indent, numericExpression.embeddings)}"
    }
    val formattedDetails =
      if (details.isEmpty) ""
      else s"($details)"

    s"$className$formattedDetails"
  }

  def visualizeStringMatcher(indent: Int, stringMatcher: StringMatcher): String = {
    val className = stringMatcher.getClass.getSimpleName
    val details = stringMatcher match {
      case stringMatcher: ExactStringMatcher => s"string = ${stringMatcher.string}"
      case stringMatcher: RegexStringMatcher => s"regex = ${stringMatcher.regex.toString}"
    }
    val formattedDetails =
      if (details.isEmpty) ""
      else s"($details)"

    s"$className$formattedDetails"
  }

  def visualizeTokenConstraint(indent: Int, tokenConstraint: TokenConstraint): String = {
    val className = tokenConstraint.getClass.getSimpleName
    val details = tokenConstraint match {
      case tokenConstraint: GreaterThan => s"lhs = ${visualizeNumericExpression(indent, tokenConstraint.lhs)}, rhs = ${visualizeNumericExpression(indent, tokenConstraint.rhs)}"
      case tokenConstraint: LessThan => s"lhs = ${visualizeNumericExpression(indent, tokenConstraint.lhs)}, rhs = ${visualizeNumericExpression(indent, tokenConstraint.rhs)}"
      case tokenConstraint: GreaterThanOrEqual => s"lhs = ${visualizeNumericExpression(indent, tokenConstraint.lhs)}, rhs = ${visualizeNumericExpression(indent, tokenConstraint.rhs)}"
      case tokenConstraint: LessThanOrEqual => s"lhs = ${visualizeNumericExpression(indent, tokenConstraint.lhs)}, rhs = ${visualizeNumericExpression(indent, tokenConstraint.rhs)}"
      case tokenConstraint: Equal => s"lhs = ${visualizeNumericExpression(indent, tokenConstraint.lhs)}, rhs = ${visualizeNumericExpression(indent, tokenConstraint.rhs)}"
      case tokenConstraint: NotEqual => s"lhs = ${visualizeNumericExpression(indent, tokenConstraint.lhs)}, rhs = ${visualizeNumericExpression(indent, tokenConstraint.rhs)}"
      case TokenWildcard => ""
      case tokenConstraint: WordConstraint => s"matcher = ${visualizeStringMatcher(indent, tokenConstraint.matcher)}"
      case tokenConstraint: LemmaConstraint => s"matcher = ${visualizeStringMatcher(indent, tokenConstraint.matcher)}"
      case tokenConstraint: TagConstraint => s"matcher = ${visualizeStringMatcher(indent, tokenConstraint.matcher)}"
      case tokenConstraint: EntityConstraint => s"matcher = ${visualizeStringMatcher(indent, tokenConstraint.matcher)}"
      case tokenConstraint: ChunkConstraint => s"matcher = ${visualizeStringMatcher(indent, tokenConstraint.matcher)}"
      case tokenConstraint: NormConstraint => s"matcher = ${visualizeStringMatcher(indent, tokenConstraint.matcher)}"
      case tokenConstraint: IncomingConstraint => s"matcher = ${visualizeStringMatcher(indent, tokenConstraint.matcher)}, graphName = ${tokenConstraint.graphName}"
      case tokenConstraint: OutgoingConstraint => s"matcher = ${visualizeStringMatcher(indent, tokenConstraint.matcher)}, graphName = ${tokenConstraint.graphName}"
      case tokenConstraint: MentionConstraint => s"matcher = ${visualizeStringMatcher(indent, tokenConstraint.matcher)}, arg = ${tokenConstraint.arg.toString}"
      case tokenConstraint: NegatedConstraint => s"constraint = ${visualizeTokenConstraint(indent, tokenConstraint.constraint)}"
      case tokenConstraint: ConjunctiveConstraint => s"lhs = ${visualizeTokenConstraint(indent, tokenConstraint.lhs)}, rhs = ${visualizeTokenConstraint(indent, tokenConstraint.rhs)}"
      case tokenConstraint: DisjunctiveConstraint => s"lhs = ${visualizeTokenConstraint(indent, tokenConstraint.lhs)}, rhs = ${visualizeTokenConstraint(indent, tokenConstraint.rhs)}"
    }
    val formattedDetails =
      if (details.isEmpty) ""
      else s"($details)"

    s"$className$formattedDetails"
  }

  def visualizeInst(indent: Int, inst: Inst, width: Int, depth: Int): String = {
    val posIdRight = inst.getPosId.toString
    val posIdLeft = " " * (width - posIdRight.length)
    val posId = posIdLeft + posIdRight
    val description = getDescription(indent, inst)
    val children = getChildren(inst)
    val links =
        if (children.isEmpty) ""
        else children.map { case InstChild(name, child, wide) =>
          if (wide) s"==$name=> ${child.getPosId}"
          else s"--$name-> ${child.getPosId}"
        }.mkString(", ", ", ", "")
    val tab = "  " * depth
    val visualization = s"$posId. $tab$description$links"

    visualization
  }

  def getDescription(indent: Int, inst: Inst): String = {
    val posId = inst.getPosId
    val name = inst.getClass.getSimpleName
    val stringEmpty = ""
    val details = inst match {
      case Done => stringEmpty
      case inst: Pass => stringEmpty
      case inst: Split => stringEmpty
      case inst: SaveStart => s"name = ${inst.name}"
      case inst: SaveEnd => s"name = ${inst.name}"
      case inst: MatchToken => s"c = ${visualizeTokenConstraint(indent, inst.c)}"
      case inst: MatchMention => s"m = ${visualizeStringMatcher(indent, inst.m)}, name = ${inst.name}, arg = ${inst.arg}"
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

  def visualizeTokenPattern(indent: Int, tokenPattern: TokenPattern): String = {
    val className = tokenPattern.getClass.getSimpleName
    val details = "..."
    val formattedDetails =
      if (details.isEmpty) ""
      else s"($details)"

    s"$className$formattedDetails"
  }

  def assignDepths(depth: Int, index: Int, insts: Array[Inst], depths: Array[Int]): Array[Int] = {
    // Do a loop inside with just depth and index
    val inst = insts(index)

    if (depths(index) == -1 || depth < depths(index)) {
      depths(index) = depth

      inst match {
        case inst: MatchLookAhead =>
          val namedChildren = getChildren(inst)

          namedChildren.foreach { case InstChild(name, child, wide) =>
            if (name != "start")
              assignDepths(depth, child.getPosId, insts, depths)
            else
              assignDepths(depth + 1, child.getPosId, insts, depths)
          }
        case inst: MatchLookBehind =>
          val namedChildren = getChildren(inst)

          namedChildren.foreach { case InstChild(name, child, wide) =>
            if (name != "start")
              assignDepths(depth, child.getPosId, insts, depths)
            else
              assignDepths(depth + 1, child.getPosId, insts, depths)
          }
        case inst =>
          val children = getChildren(inst).map(_.inst)

          children.foreach { child =>
            assignDepths(depth, child.getPosId, insts, depths)
          }
      }
    }
    depths
  }

  def extractTokenPattern(indent: Int, tokenPattern: TokenPattern): Seq[(String, String)] = {
    val sortedInsts = extractInst(tokenPattern.start)
    val depths = assignDepths(0, 1, sortedInsts.toArray, Array.fill(sortedInsts.length)(-1))
    val resortedInsts = (sortedInsts.tail :+ sortedInsts.head).toArray
    val width = (resortedInsts.length - 1).toString.length
    val visualization = resortedInsts.indices.map { index =>
      visualizeInst(indent, resortedInsts(index), width, depths(resortedInsts(index).getPosId))
    }.mkString("\n")

    Seq(("", visualization))
  }

  def extractTokenExtractor(indent: Int, tokenExtractor: TokenExtractor): Seq[(String, String)] = {
    extractTokenPattern(indent, tokenExtractor.pattern).map { case (name, value) =>
      (s"pattern:$name", value)
    }
  }

  def visualizeTokenExtractor(indent: Int, tokenExtractor: TokenExtractor): String = {
    val className = tokenExtractor.getClass.getSimpleName
    val details = Seq(
      s"name = ${tokenExtractor.name}",
      s"pattern = ${visualizeTokenPattern(indent, tokenExtractor.pattern)}"
    ).mkString(", ")

    // This is all on one line because it is used within other extractors.
    s"$className($details)"
  }

  def visualizeTokenExtractor(tokenExtractor: TokenExtractor): String = {
    val extractions = extractTokenPattern(0, tokenExtractor.pattern).map { case (name, value) =>
      (s"pattern:$name", value)
    }
    val string = StringUtils.viaPrintWriter { printWriter =>
      val string = visualizeTokenExtractor(0, tokenExtractor)

      pwPrintln(printWriter, 0, string)
      extractions.foreach { case (name, string) =>
        pwPrintln(printWriter, 0, name)
        pwPrintln(printWriter, 0 + 1, string)
      }
    }

    string
  }

  def visualizeGraphPatternNode(indent: Int, graphPatternNode: GraphPatternNode): String = {
    val className = graphPatternNode.getClass.getSimpleName
    val details = graphPatternNode match {
      case OutgoingWildcard => ""
      case IncomingWildcard => ""
      case graphPatternNode: OutgoingGraphPattern => s"matcher = ${visualizeStringMatcher(indent, graphPatternNode.matcher)}"
      case graphPatternNode: IncomingGraphPattern => s"matcher = ${visualizeStringMatcher(indent, graphPatternNode.matcher)}"
      case graphPatternNode: ConcatGraphPattern => s"lhs = ${visualizeGraphPatternNode(indent, graphPatternNode.lhs)}, rhs = ${visualizeGraphPatternNode(indent, graphPatternNode.rhs)}"
      case graphPatternNode: DisjunctiveGraphPattern => s"lhs = ${visualizeGraphPatternNode(indent, graphPatternNode.lhs)}, rhs = ${visualizeGraphPatternNode(indent, graphPatternNode.rhs)}"
      case graphPatternNode: TokenConstraintGraphPattern => s"constraint = ${visualizeTokenConstraint(indent, graphPatternNode.constraint)}"
      case graphPatternNode: LookaroundGraphPattern => s"lookaround = ${visualizeGraphPatternNode(indent, graphPatternNode.lookaround)}, negative = ${graphPatternNode.negative.toString}"
      case graphPatternNode: OptionalGraphPattern => s"pattern = ${visualizeGraphPatternNode(indent, graphPatternNode.pattern)}"
      case graphPatternNode: KleeneGraphPattern => s"pattern = ${visualizeGraphPatternNode(indent, graphPatternNode.pattern)}"
    }
    val formattedDetails =
      if (details.isEmpty) ""
      else s"($details)"

    s"$className$formattedDetails"
  }

  def visualizeArgumentQuantifier(indent: Int, argumentQuantifier: ArgumentQuantifier): String = {
    val className = argumentQuantifier.getClass.getSimpleName
    val details = argumentQuantifier match {
      case NullQuantifier => ""
      case argumentQuantifier: ExactQuantifier => s"reps = ${argumentQuantifier.reps.toString}"
      case argumentQuantifier: RangedQuantifier => s"minRepeat = ${argumentQuantifier.minRepeat.toString}, maxRepeat = ${argumentQuantifier.maxRepeat.toString}"
    }
    val formattedDetails =
      if (details.isEmpty) ""
      else s"($details)"

    s"$className$formattedDetails"
  }

  def visualizeArgumentPattern(indent: Int, argumentPattern: ArgumentPattern): String = {
    val className = argumentPattern.getClass.getSimpleName
    val details = Seq(
      s"name = ${argumentPattern.name}",
      s"label = ${argumentPattern.label}",
      s"pattern = ${visualizeGraphPatternNode(indent, argumentPattern.pattern)}",
      s"required = ${argumentPattern.required.toString}",
      s"quantifier = ${visualizeArgumentQuantifier(indent, argumentPattern.quantifier)}"
    ).mkString(", ")
    val formattedDetails =
      if (details.isEmpty) ""
      else s"($details)"

    s"$className$formattedDetails"
  }

  def visualizeGraphPattern(indent: Int, graphPattern: GraphPattern): String = {

    def visualizeArguments(arguments: Seq[ArgumentPattern]): String = {
      arguments.map { argument =>
        visualizeArgumentPattern(indent, argument)
      }.mkString(", ")
    }

    val className = graphPattern.getClass.getSimpleName
    val details = graphPattern match {
      case graphPattern: TriggerPatternGraphPattern =>
        s"trigger = ${visualizeTokenPattern(indent, graphPattern.trigger)}, arguments = [${visualizeArguments(graphPattern.arguments)}]"
      case graphPattern: TriggerMentionGraphPattern =>
        s"triggerLabel = ${graphPattern.triggerLabel}, arguments = [${visualizeArguments(graphPattern.arguments)}]"
      case graphPattern: RelationGraphPattern =>
        s"anchorName = ${graphPattern.anchorName}, anchorLabel = ${graphPattern.anchorLabel}, arguments = [${visualizeArguments(graphPattern.arguments)}]"
    }
    val formattedDetails = s"($details)"

    s"$className$formattedDetails"
  }

  def extractGraphPattern(indent: Int, graphPattern: GraphPattern): Seq[(String, String)] = {
    graphPattern match {
      case graphPattern: TriggerPatternGraphPattern =>
        extractTokenPattern(indent, graphPattern.trigger).map { case (name, value) =>
          (s"trigger:$name", value)
        }
      case graphPattern: TriggerMentionGraphPattern => Seq.empty
      case graphPattern: RelationGraphPattern => Seq.empty
    }
  }

  def visualizeGraphExtractor(indent: Int, graphExtractor: GraphExtractor): String = {
    val className = graphExtractor.getClass.getSimpleName
    val details = Seq(
      s"name = ${graphExtractor.name}",
      s"pattern = ${visualizeGraphPattern(indent, graphExtractor.pattern)}"
    )
    val extractions = extractGraphPattern(indent, graphExtractor.pattern).map { case (name, value) =>
      (s"pattern:$name", value)
    }
    val string = StringUtils.viaPrintWriter { printWriter =>
      pwPrintln(printWriter, indent, s"$className(")
      details.zipWithIndex.foreach { case (detail, index) =>
        if (index != details.length - 1)
          pwPrintln(printWriter, indent + 1, s"$detail,")
        else
          pwPrintln(printWriter, indent + 1, detail)
      }
      pwPrintln(printWriter, indent, ")")
      extractions.foreach { case (name, value) =>
        pwPrintln(printWriter, indent, name)
        pwPrintln(printWriter, indent + 1, value)
      }
    }

    string
  }

  def visualizeGraphExtractor(graphExtractor: GraphExtractor): String = {
    visualizeGraphExtractor(0, graphExtractor)
  }

  def visualizeCrossSentenceExtractor(indent: Int, crossSentenceExtractor: CrossSentenceExtractor): String = {
    val className = crossSentenceExtractor.getClass.getSimpleName
    val details = Seq(
      s"name = ${crossSentenceExtractor.name}",
      s"leftWindow = ${crossSentenceExtractor.leftWindow.toString}",
      s"rightWindow = ${crossSentenceExtractor.rightWindow.toString}",
      s"anchorPattern = ${visualizeTokenExtractor(indent, crossSentenceExtractor.anchorPattern)}",
      s"neighborPattern = ${visualizeTokenExtractor(indent, crossSentenceExtractor.neighborPattern)}",
      s"anchorRole = ${crossSentenceExtractor.anchorRole}",
      s"neighborRole = ${crossSentenceExtractor.neighborRole}"
    )
    val anchorExtractions = extractTokenExtractor(indent, crossSentenceExtractor.anchorPattern).map { case (name, value) =>
      (s"anchorPattern:$name", value)
    }
    val neighborExtractions = extractTokenExtractor(indent, crossSentenceExtractor.neighborPattern).map { case (name, value) =>
      (s"neighborPattern:$name", value)
    }

    val string = StringUtils.viaPrintWriter { printWriter =>
      pwPrintln(printWriter, indent, s"$className(")
      details.zipWithIndex.foreach { case (detail, index) =>
        if (index != details.length - 1)
          pwPrintln(printWriter, indent + 1, s"$detail,")
        else
          pwPrintln(printWriter, indent + 1, detail)
      }
      pwPrintln(printWriter, indent, ")")
      anchorExtractions.foreach { case (name, value) =>
        pwPrintln(printWriter, indent, name)
        pwPrintln(printWriter, indent + 1, value)
      }
      neighborExtractions.foreach { case (name, value) =>
        pwPrintln(printWriter, indent, name)
        pwPrintln(printWriter, indent + 1, value)
      }
    }

    string
  }

  def visualizeCrossSentenceExtractor(crossSentenceExtractor: CrossSentenceExtractor): String = {
    visualizeCrossSentenceExtractor(0, crossSentenceExtractor)
  }

  override def visualize(extractor: Extractor): TextVisualization = {
    val text = extractor match {
      case tokenExtractor: TokenExtractor => visualizeTokenExtractor(tokenExtractor)
      case graphExtractor: GraphExtractor => visualizeGraphExtractor(graphExtractor)
      case crossSentenceExtractor: CrossSentenceExtractor => visualizeCrossSentenceExtractor(crossSentenceExtractor)
      case _ => throw new RuntimeException(s"Unrecognized extractor: ${extractor.toString}")
    }
    val visualization = new TextVisualization(text)

    visualization
  }
}
