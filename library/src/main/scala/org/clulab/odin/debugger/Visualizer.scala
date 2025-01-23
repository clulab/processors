package org.clulab.odin.debugger

import org.clulab.odin.impl.{CrossSentenceExtractor, Done, Extractor, GraphExtractor, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, SaveEnd, SaveStart, Split, TokenExtractor}

import scala.annotation.tailrec

abstract class Visualizer() {
  def visualize(extractor: Extractor): Unit = {
    extractor match {
      case tokenExtractor: TokenExtractor => visualizeTokenExtractor(tokenExtractor)
      case graphExtractor: GraphExtractor => visualizeGraphExtractor(graphExtractor)
      case crossSentenceExtractor: CrossSentenceExtractor => visualizeCrossSentenceExtractor(crossSentenceExtractor)
      case _ => ???
    }
  }

  def visualizeTokenExtractor(tokenExtractor: TokenExtractor): Unit = ???

  def visualizeGraphExtractor(graphExtractor: GraphExtractor): Unit = ()

  def visualizeCrossSentenceExtractor(crossSentenceExtractor: CrossSentenceExtractor): Unit = ???
}

class TextVisualizer() extends Visualizer() {

  override def visualizeTokenExtractor(tokenExtractor: TokenExtractor): Unit = {
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
      case inst: SaveStart => s"name=${inst.name}"
      case inst: SaveEnd => s"name=${inst.name}"
      case inst: MatchToken => s"c=TokenConstraint"
      case inst: MatchMention => s"m=StringMatcher, name=${inst.name}, arg=${inst.arg}"
      case inst: MatchSentenceStart => stringEmpty
      case inst: MatchSentenceEnd => stringEmpty
      case inst: MatchLookAhead => s"negative=${inst.negative}"
      case inst: MatchLookBehind => s"negative=${inst.negative}"
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

class MermainVisualizer() extends Visualizer()
