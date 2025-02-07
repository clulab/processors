package org.clulab.odin.debugger.inspector

import org.clulab.odin.debugger.{DebuggerRecord, FinishedThread}
import org.clulab.odin.debugger.debugging.DebuggingExtractorEngine
import org.clulab.odin.debugger.utils.EqualityByIdentity
import org.clulab.odin.debugger.visualization.HtmlVisualization
import org.clulab.odin.impl.ThompsonVM.SingleThread
import org.clulab.odin.impl.{Extractor, Inst}
import org.clulab.processors.Sentence
import scalatags.Text
import scalatags.Text.all._

import scala.annotation.tailrec
import scala.collection.mutable

class Inspector(val transcript: mutable.Buffer[DebuggerRecord], val finishedThreads: mutable.Buffer[FinishedThread]) {
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
    |table.bordered, table.bordered th, table.bordered td {
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
  val bordered = "bordered"
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

  def mkHtmlPage(textualVisualization: HtmlVisualization, graphicalVisualization: HtmlVisualization, instView: HtmlVisualization, threadView: HtmlVisualization): String = {
    val fragment = html(
      head(
        style,
        script(`type` := "module")("""
          import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs';
          mermaid.initialize({ startOnLoad: true });
        """)
      ),
      body(
        h2("Textual Extractor View"),
        textualVisualization.fragment,
        h2("Inst View"),
        instView.fragment,
        h2("Thread View"),
        threadView.fragment,
        h2("Graphical Extractor View"),
        graphicalVisualization.fragment,
      )
    )

    fragment.toString
  }
}

object Inspector {

  def apply(debuggingExtractorEngine: DebuggingExtractorEngine): Inspector = {
    new Inspector(debuggingExtractorEngine.transcript, debuggingExtractorEngine.finishedThreads)
  }
}