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
  val borderStyle = "border: 1px solid black; border-collapse: collapse"

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
    val fragment = table(style := borderStyle)(
      tr(
        th(style := borderStyle)("start"),
        words.map { word =>
          th(style := borderStyle)(word)
        },
        th(raw("&nbsp;"))
      ),
      words.indices.map { start =>
        tr(
          td(style := borderStyle)(words(start)),
          extraWordRange.map { tok =>
            val instAndMatchesOptSeq = findMatches(start, tok)

            td(style := borderStyle) {
              val spans = instAndMatchesOptSeq.flatMap { case (inst, matchesOpt) =>
                val color = matchesOpt match {
                  case Some(true) => "green"
                  case Some(false) => "red"
                  case None => "gray"
                }

                Seq(
                  span(style := s"color: $color")(inst.getPosId.toString),
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
    val fragment = html(body(
      h2("Textual Rule View"),
      pre(text),
      h2("Graphical Rule View"),
      h2("Inst View"),
      instView,
      h2("Thread View"),
      threadView
    ))

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

    // TODO: Assume all threads for now, but these need to be filtered as well.
    // They need to know about their sentence then
    val sentence = transcript.head.sentence
    val words = sentence.words
    val rows = finishedThreads.zipWithIndex.map { case (finishedThread, index) =>
      val singleThreads = loop(finishedThread.thread, List.empty)
      val byTok = singleThreads.groupBy(_.tok)
      val extendedRange = Range.inclusive(0, words.length)
      val tds = extendedRange.map { tok =>
        val tokSingleThreads = byTok.getOrElse(tok, List.empty)
        val posIds = tokSingleThreads.map { tokSingleThread =>
          tokSingleThread.inst.getPosId
        }.mkString(" ")

        td(style := borderStyle)(posIds)
      }
      val reasonFrag =
          if (finishedThread.mismatchReason.isDefined)
            frag(finishedThread.mismatchReason.get)
          else raw("&nbsp;")

      val result = tr(
        td(style := borderStyle)(index.toString),
        td(style := borderStyle)(finishedThread.matched.toString),
        tds,
        td(style := borderStyle)(reasonFrag)
      )

      result
    }
    val view = table(style := borderStyle)(
      tr(
        th(style := borderStyle)("index"),
        th(style := borderStyle)("matched"),
        words.map(th(style := borderStyle)(_)),
        th(style := borderStyle)(raw("&nbsp;")),
        th(style := borderStyle)("reason")
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