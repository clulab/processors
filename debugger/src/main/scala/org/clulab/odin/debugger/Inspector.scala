package org.clulab.odin.debugger

import org.clulab.odin.debugger.debugging.DebuggingExtractorEngine
import org.clulab.odin.debugger.utils.EqualityByIdentity
import org.clulab.odin.debugger.visualizer.extractor.{HtmlExtractorVisualizer, MermaidExtractorVisualizer}
import org.clulab.odin.debugger.visualizer.inst.HtmlInstVisualizer
import org.clulab.odin.debugger.visualizer.thread.HtmlThreadVisualizer
import org.clulab.odin.impl.Extractor
import org.clulab.processors.Sentence
import org.clulab.utils.FileUtils
import scalatags.Text.all._
import scalatags.generic.Frag
import scalatags.text.Builder

import scala.collection.mutable
import scala.util.Using

class Inspector(val instTranscript: mutable.Buffer[FinishedInst], val threadTranscript: mutable.Buffer[FinishedThread]) {
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
    val newInstTranscript = filterInstTranscript { debuggerRecord =>
      debuggerRecord.extractor.eq(extractor)
    }
    val newThreadTranscript = filterThreadTranscript { debuggerRecord =>
      debuggerRecord.extractor.eq(extractor)
    }

    new Inspector(newInstTranscript, newThreadTranscript)
  }

  def inspectSentence(sentence: Sentence): Inspector = {
    val newInstTranscript = filterInstTranscript { debuggerRecord =>
      debuggerRecord.sentence.eq(sentence)
    }
    val newThreadTranscript = filterThreadTranscript { debuggerRecord =>
      debuggerRecord.sentence.eq(sentence)
    }

    new Inspector(newInstTranscript, newThreadTranscript)
  }

  def filterInstTranscript(f: DebuggerRecord => Boolean): mutable.Buffer[FinishedInst] = {
    val newInstTranscript = instTranscript.filter { finishedInst =>
      f(finishedInst.debuggerRecord)
    }

    newInstTranscript
  }

  def filterThreadTranscript(f: DebuggerRecord => Boolean): mutable.Buffer[FinishedThread] = {
    val newThreadTranscript = threadTranscript.filter { finishedThread =>
      f(finishedThread.debuggerRecord)
    }

    newThreadTranscript
  }

  def filter(f: DebuggerRecord => Boolean): Inspector = {
    val newInstTranscript = filterInstTranscript(f)
    val newThreadTranscript = filterThreadTranscript(f)

    new Inspector(newInstTranscript, newThreadTranscript)
  }

  def mkHtml(fragment: Frag[Builder, String]): Frag[Builder, String] = {
    val htmlFragment = html(
      head(
        style,
        script(`type` := "module")("""
          import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs';
          mermaid.initialize({ startOnLoad: true });
        """)
      ),
      body(
        fragment
      )
    )

    htmlFragment
  }

  def asHtml(fileName: String): Inspector = {
    val htmlExtractorVisualizer = new HtmlExtractorVisualizer()
    val mermaidExtractorVisualizer = new MermaidExtractorVisualizer()
    val htmlInstVisualizer = new HtmlInstVisualizer()
    val htmlThreadVisualizer = new HtmlThreadVisualizer()

    // It is possible that the sentences for the Insts and Threads are
    // different, especially since Threads might never have gotten around
    // to match so that no Sentence would ever be recorded.
    val instEqualitySentences = instTranscript.map { finishedInst =>
      EqualityByIdentity(finishedInst.debuggerRecord.sentence)
    }
    val threadEqualitySentences = threadTranscript.map { finishedThread =>
      EqualityByIdentity(finishedThread.debuggerRecord.sentence)
    }
    val equalitySentences = (instEqualitySentences ++ threadEqualitySentences).distinct

    val sentenceFragments = equalitySentences.map { equalitySentence =>
      val sentence = equalitySentence.value.asInstanceOf[Sentence]
      val instEqualityExtractors = instTranscript
          .filter { finishedInst =>
            finishedInst.debuggerRecord.sentence.eq(sentence)
          }
          .map { finishedInst =>
            EqualityByIdentity(finishedInst.debuggerRecord.extractor)
          }
      val threadEqualityExtractors = threadTranscript
          .filter { finishedThread =>
            finishedThread.debuggerRecord.sentence.eq(sentence)
          }
          .map { finishedThread =>
            EqualityByIdentity(finishedThread.debuggerRecord.extractor)
          }
      val equalityExtractors = (instEqualityExtractors ++ threadEqualityExtractors).distinct

      val extractorFragments = equalityExtractors.map { equalityExtractor =>
        val extractor = equalityExtractor.value.asInstanceOf[Extractor]

        val newInstTranscript = instTranscript.filter { finishedInst =>
          finishedInst.debuggerRecord.sentence.eq(sentence) &&
          finishedInst.debuggerRecord.extractor.eq(extractor)
        }
        val newThreadTranscript = threadTranscript.filter { finishedThread =>
          finishedThread.debuggerRecord.sentence.eq(sentence) &&
          finishedThread.debuggerRecord.extractor.eq(extractor)
        }
        val htmlExtractorVisualization = htmlExtractorVisualizer.visualize(extractor)
        val graphicalExtractorVisualization = mermaidExtractorVisualizer.visualize(extractor)
        val instVisualization = htmlInstVisualizer.visualize(newInstTranscript)
        val threadVisualization = htmlThreadVisualizer.visualize(newThreadTranscript)

        frag(
          h2("Extractor"),
          p(extractor.name),
          h3("Textual Extractor View"),
          htmlExtractorVisualization.fragment,
          h3("Inst View"),
          instVisualization.fragment,
          h3("Thread View"),
          threadVisualization.fragment,
          h3("Graphical Extractor View"),
          graphicalExtractorVisualization.fragment
        )
      }
      val sentenceFragment = frag(
        h1("Sentence"),
        p(sentence.getSentenceText),
        extractorFragments
      )

      sentenceFragment
    }
    val bodyFragment = frag(sentenceFragments)
    val htmlPage = mkHtml(bodyFragment).toString

    Using.resource(FileUtils.printWriterFromFile(fileName)) { printWriter =>
      printWriter.println(htmlPage)
    }
    this
  }
}

object Inspector {

  def apply(debuggingExtractorEngine: DebuggingExtractorEngine): Inspector = {
    new Inspector(debuggingExtractorEngine.transcript, debuggingExtractorEngine.finishedThreads)
  }
}