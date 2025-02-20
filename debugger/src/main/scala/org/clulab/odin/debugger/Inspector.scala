package org.clulab.odin.debugger

import org.clulab.odin.debugger.debug.{DebuggerRecord, FinishedGlobalAction, FinishedInst, FinishedLocalAction, FinishedThread}
import org.clulab.odin.debugger.odin.DebuggingExtractorEngine
import org.clulab.odin.debugger.utils.EqualityByIdentity
import org.clulab.odin.debugger.visualizer.action.HtmlActionVisualizer
import org.clulab.odin.debugger.visualizer.extractor.{HtmlExtractorVisualizer, MermaidExtractorVisualizer}
import org.clulab.odin.debugger.visualizer.html.HtmlVisualizing
import org.clulab.odin.debugger.visualizer.inst.HtmlInstVisualizer
import org.clulab.odin.debugger.visualizer.rule.HtmlRuleVisualizer
import org.clulab.odin.debugger.visualizer.sentence.HtmlSentenceVisualizer
import org.clulab.odin.debugger.visualizer.thread.HtmlThreadVisualizer
import org.clulab.odin.impl.Extractor
import org.clulab.processors.{Document, Sentence}
import org.clulab.utils.FileUtils
import scalatags.Text.all._

import scala.util.Using

class Inspector(
  val extractors: Seq[Extractor],
  val instTranscript: Seq[FinishedInst],
  val threadTranscript: Seq[FinishedThread],
  val localActionTranscript: Seq[FinishedLocalAction],
  val globalActionTranscript: Seq[FinishedGlobalAction]
) extends HtmlVisualizing {

  def copy(
    extractors: Seq[Extractor] = this.extractors,
    instTranscript: Seq[FinishedInst] = this.instTranscript,
    threadTranscript: Seq[FinishedThread] = this.threadTranscript,
    localActionTranscript: Seq[FinishedLocalAction] = this.localActionTranscript,
    globalActionTranscript: Seq[FinishedGlobalAction] = this.globalActionTranscript
  ): Inspector = {
    new Inspector(extractors, instTranscript, threadTranscript, localActionTranscript, globalActionTranscript)
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

  def filterInstTranscript(f: DebuggerRecord => Boolean): Seq[FinishedInst] = {
    val newInstTranscript = instTranscript.filter { finishedInst =>
      f(finishedInst.debuggerRecord)
    }

    newInstTranscript
  }

  def filterThreadTranscript(f: DebuggerRecord => Boolean): Seq[FinishedThread] = {
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

  def mkHtml(fragment: Fragment): Fragment = {
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

    val extractorFragments = extractors.map { extractor =>
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

  def getEqualityDocuments(): Seq[EqualityByIdentity[Document]] = {
    val instEqualityDocuments = instTranscript.map { finishedInst =>
      EqualityByIdentity(finishedInst.debuggerRecord.document)
    }.distinct
    val threadEqualityDocuments = threadTranscript.map { finishedThread =>
      EqualityByIdentity(finishedThread.debuggerRecord.document)
    }.distinct
    val equalitySentences = (instEqualityDocuments ++ threadEqualityDocuments).distinct

    equalitySentences
  }

  def getEqualitySentences(document: Document): Seq[EqualityByIdentity[Sentence]] = {
    // It is possible that the sentences for the Insts and Threads are
    // different, especially since Threads might never have gotten around
    // to match so that no Sentence would ever be recorded.
    val instEqualitySentences = instTranscript.map { finishedInst =>
      EqualityByIdentity(finishedInst.debuggerRecord.sentence)
    }.distinct
    val threadEqualitySentences = threadTranscript.map { finishedThread =>
      EqualityByIdentity(finishedThread.debuggerRecord.sentence)
    }.distinct
    val equalitySentences = (instEqualitySentences ++ threadEqualitySentences).distinct

    equalitySentences
  }

  def getEqualityExtractors(sentence: Sentence): Seq[EqualityByIdentity[Extractor]] = {
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

    equalityExtractors
  }

  def inspectDynamicAsHtml(fileName: String): Inspector = {
    val htmlSentenceVisualizer = new HtmlSentenceVisualizer()
    val htmlRuleVisualizer = new HtmlRuleVisualizer()
    val htmlExtractorVisualizer = new HtmlExtractorVisualizer()
    val mermaidExtractorVisualizer = new MermaidExtractorVisualizer()
    val htmlInstVisualizer = new HtmlInstVisualizer()
    val htmlThreadVisualizer = new HtmlThreadVisualizer()
    val htmlActionVisualizer = new HtmlActionVisualizer()

    val equalityDocuments = getEqualityDocuments()
    val documentFragments = equalityDocuments.map { equalityDocument =>
      val document = equalityDocument.value
      val equalitySentences = getEqualitySentences(document)
      val sentenceFragments = equalitySentences.map { equalitySentence =>
        val sentence = equalitySentence.value.asInstanceOf[Sentence]
        val htmlSentenceVisualization = htmlSentenceVisualizer.visualize(sentence)
        val equalityExtractors = getEqualityExtractors(sentence)
        val extractorFragments = equalityExtractors.map { equalityExtractor =>
          val extractor = equalityExtractor.value.asInstanceOf[Extractor]

          val newInstTranscript = instTranscript.filter { finishedInst =>
            finishedInst.debuggerRecord.document.eq(document) &&
            finishedInst.debuggerRecord.sentence.eq(sentence) &&
            finishedInst.debuggerRecord.extractor.eq(extractor)
          }
          val newThreadTranscript = threadTranscript.filter { finishedThread =>
            finishedThread.debuggerRecord.document.eq(document) &&
            finishedThread.debuggerRecord.sentence.eq(sentence) &&
            finishedThread.debuggerRecord.extractor.eq(extractor)
          }
          val newLocalActionTranscript = localActionTranscript.filter { finishedLocalAction =>
            finishedLocalAction.debuggerRecord.document.eq(document) &&
            finishedLocalAction.debuggerRecord.sentence.eq(sentence) &&
            finishedLocalAction.debuggerRecord.extractor.eq(extractor)
          }
          val htmlRuleVisualization = htmlRuleVisualizer.visualize(extractor)
          val htmlExtractorVisualization = htmlExtractorVisualizer.visualize(extractor)
          val graphicalExtractorVisualization = mermaidExtractorVisualizer.visualize(extractor)
          val instVisualization = htmlInstVisualizer.visualize(newInstTranscript)
          val threadVisualization = htmlThreadVisualizer.visualize(newThreadTranscript)
          val actionVisualization = htmlActionVisualizer.visualizeLocal(newLocalActionTranscript)

          frag(
            h3("Extractor"),
            p(extractor.name),
            h4("Rule View"),
            htmlRuleVisualization.fragment,
            h4("Textual Extractor View"),
            htmlExtractorVisualization.fragment,
            h4("Inst View"),
            instVisualization.fragment,
            h4("Thread View"),
            threadVisualization.fragment,
            h4("Local Action View"),
            actionVisualization.fragment,
            h4("Graphical Extractor View"),
            graphicalExtractorVisualization.fragment,
          )
        }
        val sentenceFragment = frag(
          h2("Sentence"),
          p(sentence.getSentenceText),
          h3("Parse View"),
          htmlSentenceVisualization.fragment,
          extractorFragments.toSeq
        )

        sentenceFragment
      }.toSeq
      val newGlobalActionTranscript = globalActionTranscript.filter { finishedGlobalAction =>
        finishedGlobalAction.debuggerRecord.document.eq(document)
      }
      val actionVisualization = htmlActionVisualizer.visualizeGlobal(newGlobalActionTranscript)
      val documentTextFragment = document.text
          .map { text =>
            val maxLength = 100
            val shortText = text.take(maxLength)
            val ellipse = if (text.length <= maxLength) "" else "..."

            frag(s"$shortText$ellipse")
          }
          .getOrElse(
            frag(span(`class` := red)("The document text was not saved when it was parsed, for example, with keepText = true.  Please see the individual sentence texts."))
          )
      val documentFragment = frag(
        h1("Document"),
        p(documentTextFragment),
        h2("Global Action View"),
        actionVisualization.fragment,
        sentenceFragments
      )

      documentFragment
    }
    val bodyFragment = frag(documentFragments)
    val htmlPage = mkHtml(bodyFragment).toString

    Using.resource(FileUtils.printWriterFromFile(fileName)) { printWriter =>
      printWriter.println(htmlPage)
    }
    this
  }
}

object Inspector {

  def apply(debuggingExtractorEngine: DebuggingExtractorEngine): Inspector = {
    new Inspector(
      debuggingExtractorEngine.extractors,
      debuggingExtractorEngine.finishedInsts,
      debuggingExtractorEngine.finishedThreads,
      debuggingExtractorEngine.finishedLocalActions,
      debuggingExtractorEngine.finishedGlobalActions
    )
  }
}
