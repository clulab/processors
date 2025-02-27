package org.clulab.odin.debugger.debug

import org.clulab.odin.impl.Extractor
import org.clulab.processors.{Document, Sentence}

class DynamicDebuggerFilter(f: DynamicDebuggerFilter.FilterType)  {

  def apply(debuggerContext: ImmutableDebuggerContext): Boolean = f(debuggerContext)

  def sentenceFilter(sentence: Sentence): DynamicDebuggerFilter = {
    val sentenceFilter = DynamicDebuggerFilter.sentenceFilter(sentence)
    val f = (debuggerContext: ImmutableDebuggerContext) => {
      this.f(debuggerContext) && sentenceFilter(debuggerContext)
    }

    DynamicDebuggerFilter(f)
  }
}

object DynamicDebuggerFilter {
  type FilterType = ImmutableDebuggerContext => Boolean

  val trueFilter: DynamicDebuggerFilter = {
    val f = (debuggerContext: ImmutableDebuggerContext) => {
      true
    }

    DynamicDebuggerFilter(f)
  }

  def apply(f: FilterType): DynamicDebuggerFilter = new DynamicDebuggerFilter(f)

  def documentFilter(document: Document): DynamicDebuggerFilter = {
    val f = (debuggerContext: ImmutableDebuggerContext) => {
      debuggerContext.documentOpt.isDefined &&
      debuggerContext.document.eq(document)
    }

    DynamicDebuggerFilter(f)
  }

  def extractorFilter(extractor: Extractor): DynamicDebuggerFilter = {
    val f = (debuggerContext: ImmutableDebuggerContext) => {
      debuggerContext.extractorOpt.isDefined &&
      debuggerContext.extractor.eq(extractor)
    }

    DynamicDebuggerFilter(f)
  }

  def sentenceFilter(sentence: Sentence): DynamicDebuggerFilter = {
    val f = (debuggerContext: ImmutableDebuggerContext) => {
      debuggerContext.sentenceOpt.isDefined &&
      debuggerContext.sentence.eq(sentence)
    }

    DynamicDebuggerFilter(f)
  }

  def sentencesFilter(sentences: Seq[Sentence]): DynamicDebuggerFilter = {
    val f = (debuggerContext: ImmutableDebuggerContext) => {
      debuggerContext.sentenceOpt.isDefined &&
      sentences.exists { sentence =>
        sentence.eq(debuggerContext.sentence)
      }
    }

    DynamicDebuggerFilter(f)
  }

  def multiFilter(document: Document, sentence: Sentence, extractor: Extractor): DynamicDebuggerFilter = {
    val f = (debuggerContext: ImmutableDebuggerContext) => {
      debuggerContext.documentOpt.isDefined &&
      debuggerContext.document.eq(document) &&
      debuggerContext.sentenceOpt.isDefined &&
      debuggerContext.sentence.eq(sentence) &&
      debuggerContext.extractorOpt.isDefined &&
      debuggerContext.extractor.eq(extractor)
    }

    DynamicDebuggerFilter(f)
  }

  def startTokFilter(start: Int, tok: Int): DynamicDebuggerFilter = {
    val f = (debuggerContext: ImmutableDebuggerContext) => {
      debuggerContext.startOpt.isDefined &&
      debuggerContext.start ==  start &&
      debuggerContext.tokOpt.isDefined &&
      debuggerContext.tok == tok
    }

    DynamicDebuggerFilter(f)
  }
}
