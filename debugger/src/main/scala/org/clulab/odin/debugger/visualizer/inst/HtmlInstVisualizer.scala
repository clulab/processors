package org.clulab.odin.debugger.visualizer.inst

import org.clulab.odin.debugger.debug.DynamicDebuggerFilter
import org.clulab.odin.debugger.debug.finished.FinishedInst
import org.clulab.odin.debugger.utils.{EqualityByIdentity, Transcript}
import org.clulab.odin.debugger.visualization.HtmlVisualization
import org.clulab.odin.debugger.visualizer.html.HtmlVisualizing
import org.clulab.odin.impl.Inst
import org.clulab.processors.Sentence
import scalatags.Text.all._

class HtmlInstVisualizer() extends InstVisualizer with HtmlVisualizing {

  def mkInstView(transcript: Transcript[FinishedInst], sentence: Sentence): Fragment = {
    val sentenceFilter = DynamicDebuggerFilter.sentenceFilter(sentence)
    val sentenceTranscript = transcript.filter(sentenceFilter)

    def findMatches(start: Int, tok: Int): Seq[(Inst, Option[Boolean])] = {
      val startTokFilter = DynamicDebuggerFilter.startTokFilter(start, tok)
      val matchTranscript = sentenceTranscript.filter(startTokFilter)
      val trues = matchTranscript.values.filter(_.instMatch).map(_.inst).distinct.toSet
      val falses = matchTranscript.values.filter(!_.instMatch).map(_.inst).distinct.toSet
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
    val tableFragment = table(`class` := bordered)(
      tr(
        th("<start>"),
        words.map { word =>
          th(word)
        },
        th(nbsp())
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

    tableFragment
  }

  def visualize(transcript: Transcript[FinishedInst]): HtmlVisualization = {
    val allSentences = transcript.map { finishedInst =>
      EqualityByIdentity(finishedInst.debuggerContext.sentence)
    }
    val distinctSentences = allSentences.distinct
    val sentences = distinctSentences.map { equalityByIdentity =>
      equalityByIdentity.value
    }
    val htmlTables = sentences.map { mkInstView(transcript, _) }.toSeq
    val fragment = frag(htmlTables)

    new HtmlVisualization(fragment)
  }
}
