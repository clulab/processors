package org.clulab.odin.debugger.visualizer.inst

import org.clulab.odin.debugger.DebuggerRecord
import org.clulab.odin.debugger.utils.EqualityByIdentity
import org.clulab.odin.debugger.visualization.HtmlVisualization
import org.clulab.odin.impl.Inst
import org.clulab.processors.Sentence
import scalatags.Text
import scalatags.Text.all._

import scala.collection.mutable

class HtmlInstVisualizer() extends InstVisualizer {
  val bordered = "bordered"
  val green = "green"
  val red = "red"
  val gray = "gray"

  def mkHtmlTable(transcript: mutable.Buffer[DebuggerRecord], sentence: Sentence): Text.TypedTag[String] = {
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
    val fragment = table(`class` := bordered)(
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

  def visualize(transcript: mutable.Buffer[DebuggerRecord]): HtmlVisualization = {
    val allSentences = transcript
      .map { debuggerRecord =>
        EqualityByIdentity(debuggerRecord.sentence)
      }
    val distinctSentences = allSentences.distinct
    val sentences = distinctSentences
        .map { equalityByIdentity => equalityByIdentity.any.asInstanceOf[Sentence] }
    val htmlTables = sentences.map { mkHtmlTable(transcript, _) }
    val fragment = frag(htmlTables)

    new HtmlVisualization(fragment)
  }
}
