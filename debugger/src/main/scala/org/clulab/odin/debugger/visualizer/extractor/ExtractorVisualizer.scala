package org.clulab.odin.debugger.visualizer.extractor

import org.clulab.odin.debugger.visualization.Visualization
import org.clulab.odin.impl.{Done, Extractor, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, SaveEnd, SaveStart, Split}

import java.io.{PrintWriter, StringWriter}
import scala.annotation.tailrec
import scala.util.Using

abstract class ExtractorVisualizer() {
  def visualize(extractor: Extractor): Visualization

  def printToString(f: PrintWriter => Unit): String = {
    val stringWriter = new StringWriter()

    Using.resource(new PrintWriter(stringWriter))  { printWriter =>
      f(printWriter)
    }

    stringWriter.toString
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

  def extractInst(start: Inst): List[Inst] = {

    @tailrec
    def loop(todos: List[Inst], visiteds: Set[Inst], dones: List[Inst]): List[Inst] = {
      todos match {
        case Nil => dones
        case head :: tail =>
          if (visiteds(head)) loop(tail, visiteds, dones)
          else loop(getChildren(head).map(_._2) ++ tail, visiteds + head, head :: dones)
      }
    }

    val unsortedInsts = loop(List(start), Set.empty, List.empty)
    val sortedInsts = unsortedInsts.sortBy(_.getPosId)

    assert(sortedInsts.head.getPosId == 0)
    assert(sortedInsts.head == Done)
    assert(start.getPosId == 1)
    sortedInsts.tail.headOption.foreach { tailHead =>
      assert(tailHead == start)
    }

    sortedInsts
  }
}
