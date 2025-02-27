package org.clulab.odin.debugger.debug

import org.clulab.odin.impl.Extractor
import org.clulab.processors.{Document, Sentence}

class DebuggerFilter(f: StaticDebuggerContext => Boolean)  {

  def apply(debuggerContext: StaticDebuggerContext): Boolean = f(debuggerContext)
}

object DebuggerFilter {

  def apply(f: StaticDebuggerContext => Boolean): DebuggerFilter = new DebuggerFilter(f)

  def documentFilter(document: Document): DebuggerFilter = {
    val f = (debuggerContext: StaticDebuggerContext) => {
      debuggerContext.documentOpt.isDefined &&
      debuggerContext.document.eq(document)
    }

    DebuggerFilter(f)
  }

  def extractorFilter(extractor: Extractor): DebuggerFilter = {
    val f = (debuggerContext: StaticDebuggerContext) => {
      debuggerContext.extractorOpt.isDefined &&
      debuggerContext.extractor.eq(extractor)
    }

    DebuggerFilter(f)
  }

  def sentenceFilter(sentence: Sentence): DebuggerFilter = {
    val f = (debuggerContext: StaticDebuggerContext) => {
      debuggerContext.sentenceOpt.isDefined &&
      debuggerContext.sentence.eq(sentence)
    }

    DebuggerFilter(f)
  }

  def sentencesFilter(sentences: Seq[Sentence]): DebuggerFilter = {
    val f = (debuggerContext: StaticDebuggerContext) => {
      debuggerContext.sentenceOpt.isDefined &&
      sentences.exists { sentence =>
        sentence.eq(debuggerContext.sentence)
      }
    }

    DebuggerFilter(f)
  }

  def multiFilter(document: Document, sentence: Sentence, extractor: Extractor): DebuggerFilter = {
    val f = (debuggerContext: StaticDebuggerContext) => {
      debuggerContext.documentOpt.isDefined &&
      debuggerContext.document.eq(document) &&
      debuggerContext.sentenceOpt.isDefined &&
      debuggerContext.sentence.eq(sentence) &&
      debuggerContext.extractorOpt.isDefined &&
      debuggerContext.extractor.eq(extractor)
    }

    DebuggerFilter(f)
  }

  def startTokFilter(start: Int, tok: Int): DebuggerFilter = {
    val f = (debuggerContext: StaticDebuggerContext) => {
      debuggerContext.startOpt.isDefined &&
      debuggerContext.start ==  start &&
      debuggerContext.tokOpt.isDefined &&
      debuggerContext.tok == tok
    }

    DebuggerFilter(f)
  }
}
