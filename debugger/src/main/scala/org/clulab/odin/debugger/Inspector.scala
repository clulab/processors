package org.clulab.odin.debugger

import org.clulab.odin.debugger.debug.Transcript
import org.clulab.odin.debugger.debug.filter.{DynamicDebuggerFilter, DynamicInspectorFilter, StaticDebuggerFilter, StaticInspectorFilter}
import org.clulab.odin.debugger.debug.finished.{Finished, FinishedGlobalAction, FinishedInst, FinishedLocalAction, FinishedMention, FinishedThread}
import org.clulab.odin.debugger.odin.DebuggingExtractorEngine
import org.clulab.odin.debugger.utils.EqualityByIdentity
import org.clulab.odin.debugger.visualizer.action.HtmlActionVisualizer
import org.clulab.odin.debugger.visualizer.extractor.{HtmlExtractorVisualizer, MermaidExtractorVisualizer}
import org.clulab.odin.debugger.visualizer.html.HtmlVisualizing
import org.clulab.odin.debugger.visualizer.inst.HtmlInstVisualizer
import org.clulab.odin.debugger.visualizer.mention.HtmlMentionVisualizer
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
  val instTranscript: Transcript[FinishedInst],
  val threadTranscript: Transcript[FinishedThread],
  val localActionTranscript: Transcript[FinishedLocalAction],
  val globalActionTranscript: Transcript[FinishedGlobalAction],
  val mentionTranscript: Transcript[FinishedMention]
) extends HtmlVisualizing {

  def copy(
    extractors: Seq[Extractor] = this.extractors,
    instTranscript: Transcript[FinishedInst] = this.instTranscript,
    threadTranscript: Transcript[FinishedThread] = this.threadTranscript,
    localActionTranscript: Transcript[FinishedLocalAction] = this.localActionTranscript,
    globalActionTranscript: Transcript[FinishedGlobalAction] = this.globalActionTranscript,
    mentionTranscript: Transcript[FinishedMention] = this.mentionTranscript
  ): Inspector = {
    new Inspector(
      extractors,
      instTranscript,
      threadTranscript,
      localActionTranscript,
      globalActionTranscript,
      mentionTranscript
    )
  }

  def filter(f: StaticDebuggerFilter): Inspector = {
    val newExtractors = extractors.filter(f(_))

    copy(extractors = newExtractors)
  }

  def filter(f: DynamicDebuggerFilter): Inspector = {
    val newInstTranscript = instTranscript.filter(f)
    val newThreadTranscript = threadTranscript.filter(f)
    val newLocalActionTranscript = localActionTranscript.filter(f)
    val newGlobalActionTranscript = globalActionTranscript.filter(f)
    val newMentionTranscript = mentionTranscript.filter(f)

    copy(
      instTranscript = newInstTranscript,
      threadTranscript = newThreadTranscript,
      localActionTranscript = newLocalActionTranscript,
      globalActionTranscript = newGlobalActionTranscript,
      mentionTranscript = newMentionTranscript
    )
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

  def conditional(condition: Boolean, fragment: Fragment): Fragment = if (condition) fragment else frag()

  def inspectStaticAsHtml(fileName: String, verbose: Boolean = false): Inspector =
      inspectStaticAsHtml(fileName, if (verbose) StaticInspectorFilter.verbose else StaticInspectorFilter.concise)

  def inspectStaticAsHtml(fileName: String, filter: StaticInspectorFilter): Inspector = {
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
        conditional(filter.showRuleView(extractor), frag(
          h3("Rule View"),
          htmlRuleVisualization.fragment
        )),
        conditional(filter.showTextualView(extractor), frag(
          h3("Textual Extractor View"),
          htmlExtractorVisualization.fragment
        )),
        conditional(filter.showGraphicalView(extractor), frag(
          h3("Graphical Extractor View"),
          graphicalExtractorVisualization.fragment
        ))
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
      EqualityByIdentity(finishedInst.debuggerContext.document)
    }.distinct
    val threadEqualityDocuments = threadTranscript.map { finishedThread =>
      EqualityByIdentity(finishedThread.debuggerContext.document)
    }.distinct
    // TODO: Actions
    val mentionEqualityDocuments = mentionTranscript.map { finishedMention =>
       EqualityByIdentity(finishedMention.debuggerContext.document)
    }
    val equalityDocuments = (instEqualityDocuments ++ threadEqualityDocuments ++ mentionEqualityDocuments).distinct

    equalityDocuments
  }

  def getEqualitySentences(document: Document): Seq[EqualityByIdentity[Sentence]] = {
    // It is possible that the sentences for the Insts and Threads are
    // different, especially since Threads might never have gotten around
    // to match so that no Sentence would ever be recorded.
    val instEqualitySentences = instTranscript.map { finishedInst =>
      EqualityByIdentity(finishedInst.debuggerContext.sentence)
    }.distinct
    val threadEqualitySentences = threadTranscript.map { finishedThread =>
      EqualityByIdentity(finishedThread.debuggerContext.sentence)
    }.distinct
    // TODO, add Actions
    val mentionEqualitySentences = mentionTranscript.map { finishedMention =>
      EqualityByIdentity(finishedMention.debuggerContext.sentence)
    }
    val equalitySentences = (instEqualitySentences ++ threadEqualitySentences ++ mentionEqualitySentences).distinct

    equalitySentences
  }

  def getEqualityExtractors(sentence: Sentence): Seq[EqualityByIdentity[Extractor]] = {
    val sentenceFilter = DynamicDebuggerFilter.sentenceFilter(sentence)

    val instEqualityExtractors = instTranscript
        .filter(sentenceFilter)
        .map { finishedInst =>
          EqualityByIdentity(finishedInst.debuggerContext.extractor)
        }
    val threadEqualityExtractors = threadTranscript
        .filter(sentenceFilter)
        .map { finishedThread =>
          EqualityByIdentity(finishedThread.debuggerContext.extractor)
        }
    // TODO Actions
    val mentionEqualityExtractors = mentionTranscript
        .filter(sentenceFilter)
        .map { finishedMention =>
          EqualityByIdentity(finishedMention.debuggerContext.extractor)
        }
    val equalityExtractors = (instEqualityExtractors ++ threadEqualityExtractors ++ mentionEqualityExtractors).distinct

    equalityExtractors
  }

  def inspectDynamicAsHtml(fileName: String, verbose: Boolean = false): Inspector =
      inspectDynamicAsHtml(fileName, if (verbose) DynamicInspectorFilter.verbose else DynamicInspectorFilter.concise)

  def inspectDynamicAsHtml(fileName: String, filter: DynamicInspectorFilter): Inspector = {
    val htmlSentenceVisualizer = new HtmlSentenceVisualizer()
    val htmlRuleVisualizer = new HtmlRuleVisualizer()
    val htmlExtractorVisualizer = new HtmlExtractorVisualizer()
    val mermaidExtractorVisualizer = new MermaidExtractorVisualizer()
    val htmlInstVisualizer = new HtmlInstVisualizer()
    val htmlThreadVisualizer = new HtmlThreadVisualizer()
    val htmlActionVisualizer = new HtmlActionVisualizer()
    val htmlMentionVisualizer = new HtmlMentionVisualizer()

    val equalityDocuments = getEqualityDocuments()
    val documentFragments = equalityDocuments.map { equalityDocument =>
      val document = equalityDocument.value
      val documentFilter = DynamicDebuggerFilter.documentFilter(document)
      val equalitySentences = getEqualitySentences(document)
      val sentenceFragments = equalitySentences.map { equalitySentence =>
        val sentence = equalitySentence.value.asInstanceOf[Sentence]
        val htmlSentenceVisualization = htmlSentenceVisualizer.visualize(sentence)
        val equalityExtractors = getEqualityExtractors(sentence)
        val extractorFragments = equalityExtractors.map { equalityExtractor =>
          val extractor = equalityExtractor.value.asInstanceOf[Extractor]
          val multiFilter = DynamicDebuggerFilter.multiFilter(document, sentence, extractor)

          val newInstTranscript = instTranscript.filter(multiFilter)
          val newThreadTranscript = threadTranscript.filter(multiFilter)
          val newLocalActionTranscript = localActionTranscript.filter(multiFilter)
          val newMentionTranscript = mentionTranscript // TODO
          val htmlRuleVisualization = htmlRuleVisualizer.visualize(extractor)
          val htmlExtractorVisualization = htmlExtractorVisualizer.visualize(extractor)
          val graphicalExtractorVisualization = mermaidExtractorVisualizer.visualize(extractor)
          val instVisualization = htmlInstVisualizer.visualize(newInstTranscript)
          val threadVisualization = htmlThreadVisualizer.visualize(newThreadTranscript)
          val actionVisualization = htmlActionVisualizer.visualizeLocal(newLocalActionTranscript)
          val mentionVisualization = htmlMentionVisualizer.visualize(newMentionTranscript)

          frag(
            h3("Extractor"),
            p(extractor.name),
            conditional(filter.showRuleView(extractor, sentence), frag(
              h4("Rule View"),
              htmlRuleVisualization.fragment
            )),
            conditional(filter.showTextualView(extractor, sentence), frag(
              h4("Textual Extractor View"),
              htmlExtractorVisualization.fragment
            )),
            conditional(filter.showInstView(extractor, sentence, newInstTranscript), frag(
              h4("Inst View"),
              instVisualization.fragment
            )),
            conditional(filter.showThreadView(extractor, sentence, newThreadTranscript), frag(
              h4("Thread View"),
              threadVisualization.fragment
            )),
            conditional(filter.showMentionView(extractor, sentence, newMentionTranscript), frag(
              h4("Mention View"),
              mentionVisualization.fragment
            )),
            conditional(filter.showLocalActionView(extractor, sentence, newLocalActionTranscript), frag(
              h4("Local Action View"),
              actionVisualization.fragment
            )),
            conditional(filter.showGraphicalView(extractor, sentence), frag(
              h4("Graphical Extractor View"),
              graphicalExtractorVisualization.fragment
            ))
          )
        }
        val sentenceFragment = frag(
          h2("Sentence"),
          p(sentence.getSentenceText),
          conditional(filter.showParseView(sentence), frag(
            h3("Parse View"),
            htmlSentenceVisualization.fragment
          )),
          extractorFragments.toSeq
        )

        sentenceFragment
      }.toSeq
      val newGlobalActionTranscript = globalActionTranscript.filter(documentFilter)
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
        conditional(filter.showGlobalActionView(newGlobalActionTranscript), frag(
          h2("Global Action View"),
          actionVisualization.fragment
        )),
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
    val inspector = new Inspector(
      debuggingExtractorEngine.extractors,
      debuggingExtractorEngine.finishedInsts,
      debuggingExtractorEngine.finishedThreads,
      debuggingExtractorEngine.finishedLocalActions,
      debuggingExtractorEngine.finishedGlobalActions,
      debuggingExtractorEngine.finishedMentions
    )

    inspector
        .filter(debuggingExtractorEngine.staticDebuggerFilter)
  }
}
