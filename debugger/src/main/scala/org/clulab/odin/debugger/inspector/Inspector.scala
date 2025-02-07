package org.clulab.odin.debugger.inspector

import org.clulab.odin.debugger.{DebuggerRecord, FinishedThread}
import org.clulab.odin.debugger.debugging.DebuggingExtractorEngine
import org.clulab.odin.debugger.utils.EqualityByIdentity
import org.clulab.odin.impl.ThompsonVM.SingleThread
import org.clulab.odin.impl.{Extractor, Inst}
import org.clulab.processors.Sentence
import scalatags.Text
import scalatags.Text.all._

import scala.annotation.tailrec
import scala.collection.mutable

class Inspector(transcript: mutable.Buffer[DebuggerRecord], finishedThreads: mutable.Buffer[FinishedThread]) {
  val style = tag("style")("""
    |body {
    |  font-family: system-ui, sans-serif;
    |  font-size: 12px;
    |}
    |
    |table {
    |  font-size: 12px;
    |}
    |
    |table, th, td {
    |  border: 1px solid;
    |  border-collapse: collapse;
    |}
    |
    |.green {
    |  color: green;
    |}
    |
    |.red {
    |  color: red;
    |}
    |
    |.gray {
    |  color: gray;
    |}
    |
    |""".stripMargin
  )
  val green = "green"
  val red = "red"
  val gray = "gray"

  def inspectExtractor(extractor: Extractor): Inspector = {
    val newTranscript = transcript.filter { debuggerRecord =>
      debuggerRecord.extractor.eq(extractor)
    }

    new Inspector(newTranscript, finishedThreads) // TODO: Filter this as well
  }

  def inspectSentence(sentence: Sentence): Inspector = {
    val newTranscript = transcript.filter { debuggerRecord =>
      debuggerRecord.sentence.eq(sentence)
    }

    new Inspector(newTranscript, finishedThreads) // TODO: Filter this as well
  }

  def mkHtmlTable(sentence: Sentence): Text.TypedTag[String] = {
    val sentenceTranscript = transcript.filter { debuggerRecord =>
      debuggerRecord.sentence.eq(sentence)
    }

    def findMatches(start: Int, tok: Int): Seq[(Inst, Option[Boolean])] = {
      val matchTranscripts = sentenceTranscript.filter { debuggerRecord =>
        debuggerRecord.startOpt.contains(start) && debuggerRecord.tokOpt.contains(tok)
      }
      val trues = matchTranscripts.filter(_.matches).flatMap(_.instOpt).distinct.toSet
      val falses = matchTranscripts.filter(!_.matches).flatMap(_.instOpt).distinct.toSet
      val matches = (trues ++ falses).toSeq.sortBy(_.getPosId)

      val summary = matches.map { inst =>
        ((trues.contains(inst), falses.contains(inst))) match {
          case (true, true) => None
          case (true, false) => Some(true)
          case (false, true) => Some(false)
          case (false, false) => None
        }
      }
      matches.zip(summary)
    }
    // This needs to overshoot to match Done for the complete sentence.
    val words = sentence.words
    val extraWordRange = Range.inclusive(0, words.length)
    val fragment = table(
      tr(
        th("start"),
        words.map { word =>
          th(word)
        },
        th(raw("&nbsp;"))
      ),
      words.indices.map { start =>
        tr(
          td(words(start)),
          extraWordRange.map { tok =>
            val instAndMatchesOptSeq = findMatches(start, tok)

            td {
              val spans = instAndMatchesOptSeq.flatMap { case (inst, matchesOpt) =>
                val color = matchesOpt match {
                  case Some(true) => green
                  case Some(false) => red
                  case None => gray
                }

                Seq(
//                  span(style := s"color: $color")(inst.getPosId.toString),
                  span(`class` := color)(inst.getPosId.toString),
                  span(" ")
                )
              }

              spans
            }
          }
        )
      }
    )

    fragment
  }

  def mkHtmlPage(text: String, instView: Text.TypedTag[String], threadView: Text.TypedTag[String]): String = {
    val fragment = html(
      head(
        style
      ),
      body(
        h2("Textual Rule View"),
        pre(text),
        h2("Graphical Rule View"),
        h2("Inst View"),
        instView,
        h2("Thread View"),
        threadView
      )
    )

    fragment.toString
  }

  def mkInstView(): Seq[Text.TypedTag[String]] = {
    val allSentences = transcript
        .map { debuggerRecord =>
          EqualityByIdentity(debuggerRecord.sentence)
        }
    val distinctSentences = allSentences.distinct
    val sentences = distinctSentences
        .map { equalityByIdentity => equalityByIdentity.any.asInstanceOf[Sentence] }
    val htmlTables = sentences.map(mkHtmlTable)

    htmlTables
  }

  def mkThreadView(): Text.TypedTag[String] = {

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
        td(`class` := survivedClass)(survivedValue),
        tds,
        td(reasonFrag)
      )

      result
    }
    val view = table(
      tr(
        th("index"),
        th("survived"),
        words.map(th(_)),
        th(raw("&nbsp;")),
        th("reason")
      ),
      rows
    )

    view
  }
}

object Inspector {

  def apply(debuggingExtractorEngine: DebuggingExtractorEngine): Inspector = {
    new Inspector(debuggingExtractorEngine.transcript, debuggingExtractorEngine.finishedThreads)
  }
}