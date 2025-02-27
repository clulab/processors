package org.clulab.odin.debugger

import org.clulab.odin.debugger.debug.finished.{FinishedGlobalAction, FinishedInst, FinishedLocalAction, FinishedMention, FinishedThread}
import org.clulab.odin.debugger.debug.{DynamicDebuggerFilter, ImmutableDebuggerContext, StaticDebuggerFilter}
import org.clulab.odin.debugger.odin.DebuggingExtractorEngine
import org.clulab.odin.debugger.utils.{EqualityByIdentity, Transcript}
import org.clulab.odin.debugger.visualizer.action.HtmlActionVisualizer
import org.clulab.odin.debugger.visualizer.extractor.{HtmlExtractorVisualizer, MermaidExtractorVisualizer}
import org.clulab.odin.debugger.visualizer.html.HtmlVisualizing
import org.clulab.odin.debugger.visualizer.inst.HtmlInstVisualizer
import org.clulab.odin.debugger.visualizer.mention.HtmlMentionVisualizer
import org.clulab.odin.debugger.visualizer.rule.HtmlRuleVisualizer
import org.clulab.odin.debugger.visualizer.sentence.HtmlSentenceVisualizer
import org.clulab.odin.debugger.visualizer.thread.HtmlThreadVisualizer
import org.clulab.odin.impl.{CrossSentenceExtractor, Extractor, GraphExtractor, TokenExtractor}
import org.clulab.processors.{Document, Sentence}
import org.clulab.utils.FileUtils
import scalatags.Text.all._

import scala.util.Using

class Inspector(
  val extractors: Seq[Extractor],
  val staticDebuggerFilter: StaticDebuggerFilter,
  val instTranscript: Transcript[FinishedInst],
  val threadTranscript: Transcript[FinishedThread],
  val localActionTranscript: Transcript[FinishedLocalAction],
  val globalActionTranscript: Transcript[FinishedGlobalAction],
  val mentionTranscript: Transcript[FinishedMention]
) extends HtmlVisualizing {

  def copy(
    extractors: Seq[Extractor] = this.extractors,
    staticDebuggerFilter: StaticDebuggerFilter = this.staticDebuggerFilter,
    instTranscript: Transcript[FinishedInst] = this.instTranscript,
    threadTranscript: Transcript[FinishedThread] = this.threadTranscript,
    localActionTranscript: Transcript[FinishedLocalAction] = this.localActionTranscript,
    globalActionTranscript: Transcript[FinishedGlobalAction] = this.globalActionTranscript,
    mentionTranscript: Transcript[FinishedMention] = this.mentionTranscript
  ): Inspector = {
    new Inspector(
      extractors,
      staticDebuggerFilter,
      instTranscript,
      threadTranscript,
      localActionTranscript,
      globalActionTranscript,
      mentionTranscript
    )
  }

  // TODO: Instead, have something return the approprate filter to use.
  // It should be usable for the debugger as well.
  def inspectTokenExtractor(extractor: TokenExtractor): Inspector = {
    inspectExtractor(extractor)
  }

  def inspectGraphExtractor(extractor: GraphExtractor): Inspector = {
    inspectExtractor(extractor)
  }

  def inspectCrossSentenceExtractor(extractor: CrossSentenceExtractor): Inspector = {
    val relatedExtractors = Seq(extractor, extractor.anchorPattern, extractor.neighborPattern)
    val f = (debuggerContext: ImmutableDebuggerContext) => {
      debuggerContext.extractorOpt.isDefined &&
      relatedExtractors.exists { relatedExtractor =>
        relatedExtractor.eq(debuggerContext.extractor)
      }
    }

    filter(DynamicDebuggerFilter(f))
  }

  def inspectExtractor(extractor: Extractor): Inspector = {
    filter(DynamicDebuggerFilter.extractorFilter(extractor))
  }

  def inspectSentence(sentence: Sentence): Inspector = {
    filter(DynamicDebuggerFilter.sentenceFilter(sentence))
  }

  def inspectSentences(sentences: Seq[Sentence]): Inspector = {
    filter(DynamicDebuggerFilter.sentencesFilter(sentences))
  }

  def filterInstTranscript(f: DynamicDebuggerFilter): Transcript[FinishedInst] = {
    val newInstTranscript = instTranscript.filter(f)

    newInstTranscript
  }

  def filterThreadTranscript(f: DynamicDebuggerFilter): Transcript[FinishedThread] = {
    val newThreadTranscript = threadTranscript.filter(f)

    newThreadTranscript
  }

  def filterLocalActionTranscript(f: DynamicDebuggerFilter): Transcript[FinishedLocalAction] = {
    val newLocalActionTranscript = localActionTranscript.filter(f)

    newLocalActionTranscript
  }

  def filterMentionTranscript(f: DynamicDebuggerFilter): Transcript[FinishedMention] = {
    val newMentionTranscript = mentionTranscript.filter(f)

    newMentionTranscript
  }

  def filter(f: DynamicDebuggerFilter): Inspector = {
    val newInstTranscript = filterInstTranscript(f)
    val newThreadTranscript = filterThreadTranscript(f)
    val newLocalActionTranscript = localActionTranscript
    // val newLocalActionTranscript = filterLocalActionTranscript(f)
    val newMentionTranscript = mentionTranscript
//    val newMentionTranscript = filterMentionTranscript(f)

    // TODO
    copy(
      instTranscript = newInstTranscript,
      threadTranscript = newThreadTranscript,
      localActionTranscript = newLocalActionTranscript,
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

  def inspectStaticAsHtml(fileName: String, verbose: Boolean = false): Inspector = {
    val htmlRuleVisualizer = new HtmlRuleVisualizer()
    val htmlExtractorVisualizer = new HtmlExtractorVisualizer()
    val mermaidExtractorVisualizer = new MermaidExtractorVisualizer()

    val extractorFragments = extractors.filter(staticDebuggerFilter(_)).map { extractor =>
      val htmlRuleVisualization = htmlRuleVisualizer.visualize(extractor)
      val htmlExtractorVisualization = htmlExtractorVisualizer.visualize(extractor)
      val graphicalExtractorVisualization = mermaidExtractorVisualizer.visualize(extractor)
      val graphicalExtractorFragment =
          if (verbose) frag(
            h3("Graphical Extractor View"),
            graphicalExtractorVisualization.fragment
          )
          else frag()

      frag(
        h2("Extractor"),
        p(extractor.name),
        h3("Rule View"),
        htmlRuleVisualization.fragment,
        h3("Textual Extractor View"),
        htmlExtractorVisualization.fragment,
        graphicalExtractorFragment
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

  def inspectDynamicAsHtml(fileName: String, verbose: Boolean = false): Inspector = {
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
            h4("Rule View"),
            htmlRuleVisualization.fragment,
            h4("Textual Extractor View"),
            htmlExtractorVisualization.fragment,
            h4("Inst View"),
            instVisualization.fragment,
            h4("Thread View"),
            threadVisualization.fragment,
            h4("Mention View"),
            mentionVisualization.fragment,
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
      debuggingExtractorEngine.staticDebuggerFilter,
      debuggingExtractorEngine.finishedInsts,
      debuggingExtractorEngine.finishedThreads,
      debuggingExtractorEngine.finishedLocalActions,
      debuggingExtractorEngine.finishedGlobalActions,
      debuggingExtractorEngine.finishedMentions
    )
  }
}
