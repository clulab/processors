package org.clulab.odin.debugger.visualizer.thread

import org.clulab.odin.debugger.{DebuggerRecord, FinishedThread}
import org.clulab.odin.debugger.visualization.HtmlVisualization
import org.clulab.odin.debugger.visualizer.HtmlStyling
import org.clulab.odin.impl.ThompsonVM.SingleThread
import scalatags.Text
import scalatags.Text.all._

import scala.annotation.tailrec
import scala.collection.mutable

class HtmlThreadVisualizer() extends ThreadVisualizer with HtmlStyling {

  def mkThreadView(transcript: mutable.Buffer[DebuggerRecord], finishedThreads: mutable.Buffer[FinishedThread]): Text.TypedTag[String] = {

    @tailrec
    def loop(singleThread: SingleThread, singleThreads: List[SingleThread]): List[SingleThread] = {
      val newSingleThreads = singleThread :: singleThreads

      if (singleThread.prevThreadOpt.isEmpty) newSingleThreads
      else
        loop(singleThread.prevThreadOpt.get.asInstanceOf[SingleThread], newSingleThreads) // TODO
    }

    def startToken(finishedThread: FinishedThread): Int = {

      @tailrec
      def loop(singleThread: SingleThread): Int = {
        if (singleThread.prevThreadOpt.isEmpty) singleThread.tok
        else loop(singleThread.prevThreadOpt.get.asInstanceOf[SingleThread])
      }

      loop(finishedThread.thread)
    }

    def length(finishedThread: FinishedThread): Int = {

      @tailrec
      def loop(singleThread: SingleThread, current: Int): Int = {
        if (singleThread.prevThreadOpt.isEmpty) current
        else loop(singleThread.prevThreadOpt.get.asInstanceOf[SingleThread], current + 1)
      }

      loop(finishedThread.thread, 1)
    }

    def sortFinishedThreads(finishedThreads: mutable.Buffer[FinishedThread]): mutable.Buffer[FinishedThread] = {
      // This assumes ties are broken by the original order.
      // Recalculation of the sort key is fairly expensive, unfortunately, so there might be a better option.

      def mkSortKey(finishedThread: FinishedThread): (Int, Int, Int) = {
        (startToken(finishedThread), length(finishedThread), if (finishedThread.matched) 0 else 1)
      }

      finishedThreads.sortBy(mkSortKey)
    }

    // TODO: Assume all threads for now, but these need to be filtered as well.
    // They need to know about their sentence then
    val sentence = transcript.head.sentence
    val words = sentence.words
    val sortedFinishedThreads = sortFinishedThreads(finishedThreads)
    val rows = sortedFinishedThreads.zipWithIndex.map { case (finishedThread, index) =>
      val singleThreads = loop(finishedThread.thread, List.empty)
      val byTok = singleThreads.groupBy(_.tok)
      val extendedRange = Range.inclusive(0, words.length)
      val maxTok = byTok.keys.max
      val tds = extendedRange.map { tok =>
        val tokSingleThreads = byTok.getOrElse(tok, List.empty)
        val posIds = tokSingleThreads.map { tokSingleThread =>
          tokSingleThread.inst.getPosId
        }.mkString(" ")
        val color =
          if (tok != maxTok || finishedThread.matched) green
          else red

        td(span(`class` := color)(posIds))
      }
      val reasonFrag =
        if (finishedThread.reasonOpt.isDefined)
          frag(finishedThread.reasonOpt.get)
        else raw("&nbsp;")
      val survivedClass = if (finishedThread.survived) green else red
      val survivedValue = if (finishedThread.survived) raw("&#9745;") else raw("&#9746;")

      val result = tr(
        td((index + 1).toString),
        td(span(`class` := survivedClass)(survivedValue)),
        tds,
        td(reasonFrag)
      )

      result
    }
    val view = table(`class` := bordered)(
      tr(
        th("#"),
        th(raw("&#9744;")),
        words.map(th(_)),
        th(raw("&nbsp;")),
        th("reason")
      ),
      rows
    )

    view
  }

  override def visualize(transcript: mutable.Buffer[DebuggerRecord], finishedThreads: mutable.Buffer[FinishedThread]): HtmlVisualization = {
    val fragment = mkThreadView(transcript, finishedThreads)

    new HtmlVisualization(frag(fragment))
  }
}
