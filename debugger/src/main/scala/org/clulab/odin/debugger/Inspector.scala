package org.clulab.odin.debugger

import org.clulab.odin.debugger.debugging.DebuggingExtractorEngine
import org.clulab.odin.debugger.utils.EqualityByIdentity
import org.clulab.odin.debugger.visualizer.HtmlStyling
import org.clulab.odin.debugger.visualizer.extractor.{HtmlExtractorVisualizer, MermaidExtractorVisualizer}
import org.clulab.odin.debugger.visualizer.inst.HtmlInstVisualizer
import org.clulab.odin.debugger.visualizer.rule.HtmlRuleVisualizer
import org.clulab.odin.debugger.visualizer.thread.HtmlThreadVisualizer
import org.clulab.odin.impl.Extractor
import org.clulab.processors.Sentence
import org.clulab.utils.FileUtils
import scalatags.Text.all._
import scalatags.generic.Frag
import scalatags.text.Builder

import scala.collection.mutable
import scala.util.Using

class Inspector(val extractors2: Seq[Extractor], val instTranscript: mutable.Buffer[FinishedInst], val threadTranscript: mutable.Buffer[FinishedThread])
    extends HtmlStyling {

  def copy(
    extractors: Seq[Extractor] = this.extractors2,
    instTranscript: mutable.Buffer[FinishedInst] = this.instTranscript,
    threadTranscript: mutable.Buffer[FinishedThread] = this.threadTranscript
  ): Inspector = {
    new Inspector(extractors, instTranscript, threadTranscript)
  }

  def inspectExtractor(extractor: Extractor): Inspector = {
    val newInstTranscript = filterInstTranscript { debuggerRecord =>
      debuggerRecord.extractor.eq(extractor)
    }
    val newThreadTranscript = filterThreadTranscript { debuggerRecord =>
      debuggerRecord.extractor.eq(extractor)
    }

    copy(instTranscript = newInstTranscript, threadTranscript = newThreadTranscript)
  }

  def inspectSentence(sentence: Sentence): Inspector = {
    val newInstTranscript = filterInstTranscript { debuggerRecord =>
      debuggerRecord.sentence.eq(sentence)
    }
    val newThreadTranscript = filterThreadTranscript { debuggerRecord =>
      debuggerRecord.sentence.eq(sentence)
    }

    copy(instTranscript = newInstTranscript, threadTranscript = newThreadTranscript)
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

    copy(instTranscript = newInstTranscript, threadTranscript = newThreadTranscript)
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

  def inspectStaticAsHtml(fileName: String): Inspector = {
    val htmlRuleVisualizer = new HtmlRuleVisualizer()
    val htmlExtractorVisualizer = new HtmlExtractorVisualizer()
    val mermaidExtractorVisualizer = new MermaidExtractorVisualizer()

    val extractorFragments = extractors2.map { extractor =>
      val htmlRuleVisualization = htmlRuleVisualizer.visualize(extractor)
      val htmlExtractorVisualization = htmlExtractorVisualizer.visualize(extractor)
      val graphicalExtractorVisualization = mermaidExtractorVisualizer.visualize(extractor)

      frag(
        h2("Extractor"),
        p(extractor.name),
        h3("Rule View"),
        htmlRuleVisualization.fragment,
        h3("Textual Extractor View"),
        htmlExtractorVisualization.fragment,
        h3("Graphical Extractor View"),
        graphicalExtractorVisualization.fragment
      )
    }
    val bodyFragment = frag(
      h1("Extractors"),
      extractorFragments
    )
    val htmlPage = mkHtml(bodyFragment).toString

    Using.resource(FileUtils.printWriterFromFile(fileName)) { printWriter =>
      printWriter.println(htmlPage)
    }
    this
  }

  def inspectDynamicAsHtml(fileName: String): Inspector = {
    val htmlRuleVisualizer = new HtmlRuleVisualizer()
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
        val htmlRuleVisualization = htmlRuleVisualizer.visualize(extractor)
        val htmlExtractorVisualization = htmlExtractorVisualizer.visualize(extractor)
        val graphicalExtractorVisualization = mermaidExtractorVisualizer.visualize(extractor)
        val instVisualization = htmlInstVisualizer.visualize(newInstTranscript)
        val threadVisualization = htmlThreadVisualizer.visualize(newThreadTranscript)

        frag(
          h2("Extractor"),
          p(extractor.name),
          h3("Rule View"),
          htmlRuleVisualization.fragment,
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
    new Inspector(debuggingExtractorEngine.extractors, debuggingExtractorEngine.transcript, debuggingExtractorEngine.finishedThreads)
  }
}