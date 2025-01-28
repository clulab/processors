package org.clulab.odin.debugger.visualizer

import org.clulab.odin.impl.{Addition, ChunkConstraint, ConjunctiveConstraint, Constant, DisjunctiveConstraint, Division, Done, EmbeddingsResource, EntityConstraint, Equal, EuclideanQuotient, EuclideanRemainder, ExactStringMatcher, GreaterThan, GreaterThanOrEqual, IncomingConstraint, Inst, LemmaConstraint, LessThan, LessThanOrEqual, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, MentionConstraint, Multiplication, NegatedConstraint, NegativeExpression, NormConstraint, NotEqual, NumericExpression, OutgoingConstraint, Pass, RegexStringMatcher, SaveEnd, SaveStart, SimilarityConstraint, Split, StringMatcher, Subtraction, TagConstraint, TokenConstraint, TokenExtractor, TokenWildcard, WordConstraint}

import scala.annotation.tailrec

class TextVisualizer() extends Visualizer() {

  def visualize(embeddingsResource: EmbeddingsResource): String = {
    val name = embeddingsResource.getClass.getSimpleName
    val details = s"p = ${embeddingsResource.p}"
    val formattedDetails =
      if (details.isEmpty) ""
      else s"($details)"

    s"$name$formattedDetails"
  }

  def visualize(numericExpression: NumericExpression): String = {
    val name = numericExpression.getClass.getSimpleName
    val details = numericExpression match {
      case numericExpression: Constant => s"value = ${numericExpression.value}"
      case numericExpression: Addition => s"lhs = ${visualize(numericExpression.lhs)}, rhs = ${visualize(numericExpression.rhs)}"
      case numericExpression: Subtraction => s"lhs = ${visualize(numericExpression.lhs)}, rhs = ${visualize(numericExpression.rhs)}"
      case numericExpression: Multiplication => s"lhs = ${visualize(numericExpression.lhs)}, rhs = ${visualize(numericExpression.rhs)}"
      case numericExpression: Division => s"lhs = ${visualize(numericExpression.lhs)}, rhs = ${visualize(numericExpression.rhs)}"
      case numericExpression: EuclideanQuotient => s"lhs = ${visualize(numericExpression.lhs)}, rhs = ${visualize(numericExpression.rhs)}"
      case numericExpression: EuclideanRemainder => s"lhs = ${visualize(numericExpression.lhs)}, rhs = ${visualize(numericExpression.rhs)}"
      case numericExpression: NegativeExpression => s"expr = ${visualize(numericExpression.expr)}"
      case numericExpression: SimilarityConstraint => s"w1 = ${numericExpression.w1}, embeddings = ${visualize(numericExpression.embeddings)}"
    }
    val formattedDetails =
      if (details.isEmpty) ""
      else s"($details)"

    s"$name$formattedDetails"
  }

  def visualize(stringMatcher: StringMatcher): String = {
    val name = stringMatcher.getClass.getSimpleName
    val details = stringMatcher match {
      case stringMatcher: ExactStringMatcher => s"string = ${stringMatcher.string}"
      case stringMatcher: RegexStringMatcher => s"regex = ${stringMatcher.regex.toString}"
    }
    val formattedDetails =
      if (details.isEmpty) ""
      else s"($details)"

    s"$name$formattedDetails"
  }

  def visualize(tokenConstraint: TokenConstraint): String = {
    val name = tokenConstraint.getClass.getSimpleName
    val details = tokenConstraint match {
      case tokenConstraint: GreaterThan => s"lhs = ${visualize(tokenConstraint.lhs)}, rhs = ${visualize(tokenConstraint.rhs)}"
      case tokenConstraint: LessThan => s"lhs = ${visualize(tokenConstraint.lhs)}, rhs = ${visualize(tokenConstraint.rhs)}"
      case tokenConstraint: GreaterThanOrEqual => s"lhs = ${visualize(tokenConstraint.lhs)}, rhs = ${visualize(tokenConstraint.rhs)}"
      case tokenConstraint: LessThanOrEqual => s"lhs = ${visualize(tokenConstraint.lhs)}, rhs = ${visualize(tokenConstraint.rhs)}"
      case tokenConstraint: Equal => s"lhs = ${visualize(tokenConstraint.lhs)}, rhs = ${visualize(tokenConstraint.rhs)}"
      case tokenConstraint: NotEqual => s"lhs = ${visualize(tokenConstraint.lhs)}, rhs = ${visualize(tokenConstraint.rhs)}"
      case TokenWildcard => ""
      case tokenConstraint: WordConstraint => s"matcher = ${visualize(tokenConstraint.matcher)}"
      case tokenConstraint: LemmaConstraint => s"matcher = ${visualize(tokenConstraint.matcher)}"
      case tokenConstraint: TagConstraint => s"matcher = ${visualize(tokenConstraint.matcher)}"
      case tokenConstraint: EntityConstraint => s"matcher = ${visualize(tokenConstraint.matcher)}"
      case tokenConstraint: ChunkConstraint => s"matcher = ${visualize(tokenConstraint.matcher)}"
      case tokenConstraint: NormConstraint => s"matcher = ${visualize(tokenConstraint.matcher)}"
      case tokenConstraint: IncomingConstraint => s"matcher = ${visualize(tokenConstraint.matcher)}, graphName = ${tokenConstraint.graphName}"
      case tokenConstraint: OutgoingConstraint => s"matcher = ${visualize(tokenConstraint.matcher)}, graphName = ${tokenConstraint.graphName}"
      case tokenConstraint: MentionConstraint => s"matcher = ${visualize(tokenConstraint.matcher)}, arg = ${tokenConstraint.arg.toString}"
      case tokenConstraint: NegatedConstraint => s"constraint = ${visualize(tokenConstraint.constraint)}"
      case tokenConstraint: ConjunctiveConstraint => s"lhs = ${visualize(tokenConstraint.lhs)}, rhs = ${visualize(tokenConstraint.rhs)}"
      case tokenConstraint: DisjunctiveConstraint => s"lhs = ${visualize(tokenConstraint.lhs)}, rhs = ${visualize(tokenConstraint.rhs)}"
    }
    val formattedDetails =
      if (details.isEmpty) ""
      else s"($details)"

    s"$name$formattedDetails"
  }

  override def visualize(tokenExtractor: TokenExtractor): Unit = {
    val start: Inst = tokenExtractor.pattern.start
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

    val className = tokenExtractor.getClass.getSimpleName
    val label = s"$className(${tokenExtractor.name})"

    println(label)
    resortedInsts.foreach(visualize)
  }

  def getDescription(inst: Inst): String = {
    val posId = inst.getPosId
    val name = inst.getClass.getSimpleName
    val stringEmpty = ""
    val details = inst match {
      case Done => stringEmpty
      case inst: Pass => stringEmpty
      case inst: Split => stringEmpty
      case inst: SaveStart => s"name = ${inst.name}"
      case inst: SaveEnd => s"name = ${inst.name}"
      case inst: MatchToken => s"c = ${visualize(inst.c)}"
      case inst: MatchMention => s"m = ${visualize(inst.m)}, name = ${inst.name}, arg = ${inst.arg}"
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

  def visualize(inst: Inst): Unit = {
    val posId = inst.getPosId
    val description = getDescription(inst)
    val children = getChildren(inst)
    val links =
      if (children.isEmpty) ""
      else children.map { case (name, child) =>
        s"--$name-> ${child.getPosId}"
      }.mkString(", ", ", ", "")
    val visualization = s"$posId. $description$links"

    println(visualization)
  }
}
