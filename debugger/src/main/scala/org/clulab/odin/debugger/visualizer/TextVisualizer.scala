package org.clulab.odin.debugger.visualizer

import org.clulab.odin.impl.{Addition, ArgumentPattern, ArgumentQuantifier, ChunkConstraint, ConcatGraphPattern, ConjunctiveConstraint, Constant, CrossSentenceExtractor, DisjunctiveConstraint, DisjunctiveGraphPattern, Division, Done, EmbeddingsResource, EntityConstraint, Equal, EuclideanQuotient, EuclideanRemainder, ExactQuantifier, ExactStringMatcher, Extractor, GraphExtractor, GraphPattern, GraphPatternNode, GreaterThan, GreaterThanOrEqual, IncomingConstraint, IncomingGraphPattern, IncomingWildcard, Inst, KleeneGraphPattern, LemmaConstraint, LessThan, LessThanOrEqual, LookaroundGraphPattern, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, MentionConstraint, Multiplication, NegatedConstraint, NegativeExpression, NormConstraint, NotEqual, NullQuantifier, NumericExpression, OptionalGraphPattern, OutgoingConstraint, OutgoingGraphPattern, OutgoingWildcard, Pass, RangedQuantifier, RegexStringMatcher, RelationGraphPattern, SaveEnd, SaveStart, SimilarityConstraint, Split, StringMatcher, Subtraction, TagConstraint, TokenConstraint, TokenConstraintGraphPattern, TokenExtractor, TokenPattern, TokenWildcard, TriggerMentionGraphPattern, TriggerPatternGraphPattern, WordConstraint}

import scala.annotation.tailrec

class TextVisualizer() extends Visualizer() {

  def println(indent: Int, string: String): Unit = {
    val spaces = "  " * indent
    val spacedString = string.replaceAll("\n", "\n" + spaces)

    System.out.println(s"$spaces$spacedString")
  }

  def visualizeEmbeddingsResource(indent: Int, embeddingsResource: EmbeddingsResource): String = {
    val className = embeddingsResource.getClass.getSimpleName
    val details = s"p = ${embeddingsResource.p}"
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

  def visualizeInst(indent: Int, inst: Inst): String = {
    val posId = inst.getPosId
    val description = getDescription(indent, inst)
    val children = getChildren(inst)
    val links =
      if (children.isEmpty) ""
      else children.map { case (name, child) =>
        s"--$name-> ${child.getPosId}"
      }.mkString(", ", ", ", "")
    val visualization = s"$posId. $description$links"

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

  def getChildren(inst: Inst): List[(String, Inst)] = {
    val nexts = Option(inst.getNext).map { next =>
      "next" -> next
    }.toList

    val others = inst match {
      case Done => List.empty
      case inst: Pass => List.empty
      case inst: Split => List("lhs" -> inst.lhs, "rhs" -> inst.rhs)
      case inst: SaveStart => List.empty
      case inst: SaveEnd => List.empty
      case inst: MatchToken => List.empty
      case inst: MatchMention => List.empty
      case inst: MatchSentenceStart => List.empty
      case inst: MatchSentenceEnd => List.empty
      case inst: MatchLookAhead => List("start" -> inst.start)
      case inst: MatchLookBehind => List("start" -> inst.start)
    }
    others ++ nexts
  }

  def visualizeTokenPattern(indent: Int, tokenPattern: TokenPattern): String = {
    val className = tokenPattern.getClass.getSimpleName
    val details = "..."
    val formattedDetails =
      if (details.isEmpty) ""
      else s"($details)"

    s"$className$formattedDetails"
  }

  def extractTokenPattern(indent: Int, tokenPattern: TokenPattern): Seq[(String, String)] = {
    val start: Inst = tokenPattern.start
    val unsortedInsts = {

      @tailrec
      def loop(todos: List[Inst], dones: List[Inst]): List[Inst] = {
        todos match {
          case Nil => dones
          case head :: tail =>
            if (dones.contains(head)) loop(tail, dones)
            else loop(getChildren(head).map(_._2) ++ tail, head :: dones)
        }
      }

      loop(List(start), List.empty)
    }
    val sortedInsts = unsortedInsts.sortBy(_.getPosId)

    assert(sortedInsts.head.getPosId == 0)
    assert(sortedInsts.head == Done)
    assert(start.getPosId == 1)
    sortedInsts.tail.headOption.foreach { tailHead =>
      assert(tailHead == start)
    }

    val resortedInsts = sortedInsts.tail :+ sortedInsts.head
    val visualization = resortedInsts.map { inst =>
      visualizeInst(indent, inst)
    }.mkString("\n")

    Seq(("", visualization))
  }

  def visualizeTokenExtractor(indent: Int, tokenExtractor: TokenExtractor): Unit = {
    val className = tokenExtractor.getClass.getSimpleName
    val details = s"name = ${tokenExtractor.name}, pattern = ${visualizeTokenPattern(indent, tokenExtractor.pattern)}"
    val extractions = extractTokenPattern(indent, tokenExtractor.pattern).map { case (name, value) =>
      (s"pattern:$name", value)
    }

    println(indent, s"$className($details)")
    extractions.foreach { case (name, string) =>
      println(indent, name)
      println(indent + 1, string)
    }
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
        s"trigger = ${
          visualizeTokenPattern(indent, graphPattern.trigger)
        }, arguments = [${visualizeArguments(graphPattern.arguments)}]"
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
      case graphPattern: TriggerMentionGraphPattern => Seq.empty // TODO
      case graphPattern: RelationGraphPattern => Seq.empty // TODO
    }

  }

  def visualizeGraphExtractor(indent: Int, graphExtractor: GraphExtractor): Unit = {
    val className = graphExtractor.getClass.getSimpleName
    val details = s"name = ${graphExtractor.name}, pattern = ${visualizeGraphPattern(indent, graphExtractor.pattern)}..."
    val extractions = extractGraphPattern(indent, graphExtractor.pattern).map { case (name, value) =>
      (s"pattern:$name", value)
    }

    println(indent, s"$className($details)")
    extractions.foreach { case (name, value) =>
      println(indent, name)
      println(indent + 1, value)
    }
    println(indent + 1, visualizeGraphPattern(indent + 1, graphExtractor.pattern))
  }

  def visualizeCrossSentenceExtractor(indent: Int, crossSentenceExtractor: CrossSentenceExtractor): Unit = {
    val className = crossSentenceExtractor.getClass.getSimpleName
    val details = Seq(
      s"name = ${crossSentenceExtractor.name}",
      s"leftWindow = ${crossSentenceExtractor.leftWindow.toString}",
      s"rightWindow = ${crossSentenceExtractor.rightWindow.toString}",
      s"anchorPattern = ...",
      s"neighborPattern = ...",
      s"anchorRole = ${crossSentenceExtractor.anchorRole}",
      s"neighborRole = ${crossSentenceExtractor.neighborRole}"
    ).mkString(", ")

    println(indent, s"$className($details)")
    println(indent, "anchorPattern:")
    visualizeTokenExtractor(indent + 1, crossSentenceExtractor.anchorPattern)
    println(indent, "neighborPattern:")
    visualizeTokenExtractor(indent + 1, crossSentenceExtractor.neighborPattern)
  }

  override def visualize(extractor: Extractor): Unit = {
    extractor match {
      case tokenExtractor: TokenExtractor => visualizeTokenExtractor(0, tokenExtractor)
      case graphExtractor: GraphExtractor => visualizeGraphExtractor(0, graphExtractor)
      case crossSentenceExtractor: CrossSentenceExtractor => visualizeCrossSentenceExtractor(0, crossSentenceExtractor)
      case _ => ???
    }
  }
}
