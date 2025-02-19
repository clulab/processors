package org.clulab.odin.debugger.visualizer.thread

import org.clulab.odin.debugger.utils.EqualityByIdentity
import org.clulab.odin.debugger.FinishedThread
import org.clulab.odin.debugger.visualization.HtmlVisualization
import org.clulab.odin.debugger.visualizer.html.HtmlVisualizing
import org.clulab.odin.impl.ThompsonVM.SingleThread
import org.clulab.processors.Sentence
import scalatags.Text
import scalatags.Text.all._

import scala.annotation.tailrec
import scala.collection.mutable

class HtmlThreadVisualizer() extends ThreadVisualizer with HtmlVisualizing {

  def mkThreadView(transcript: mutable.Buffer[FinishedThread], sentence: Sentence): Text.TypedTag[String] = {

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
        (startToken(finishedThread), length(finishedThread), if (finishedThread.instMatch) 0 else 1)
      }

      finishedThreads.sortBy(mkSortKey)
    }

    // TODO: Assume all threads for now, but these need to be filtered as well.
    // They need to know about their sentence then
    val sentenceTranscript = transcript.filter { finishedThread =>
      finishedThread.debuggerRecord.sentence.eq(sentence)
    }
    val words = sentence.words
    val sortedFinishedThreads = sortFinishedThreads(sentenceTranscript)
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
          if (tok != maxTok || finishedThread.instMatch) green
          else red

        td(span(`class` := color)(posIds))
      }
      val reasonFrag = finishedThread.threadMatch.reason
      val survivedClass = if (finishedThread.threadMatch.matches) green else red
      val survivedValue = if (finishedThread.threadMatch.matches) raw("&#9745;") else raw("&#9746;")

      val result = tr(
        td((index + 1).toString),
        td(span(`class` := survivedClass)(survivedValue)),
        tds,
        td(reasonFrag)
      )

      result
    }.toSeq
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

  override def visualize(transcript: mutable.Buffer[FinishedThread]): HtmlVisualization = {
    val allSentences = transcript.map { finishedThread =>
      EqualityByIdentity(finishedThread.debuggerRecord.sentence)
    }
    val distinctSentences = allSentences.distinct
    val sentences = distinctSentences.map { equalityByIdentity =>
      equalityByIdentity.value.asInstanceOf[Sentence]
    }
    val htmlTables = sentences.map { mkThreadView(transcript, _) }.toSeq
    val fragment = frag(htmlTables)

    new HtmlVisualization(fragment)
  }
}
