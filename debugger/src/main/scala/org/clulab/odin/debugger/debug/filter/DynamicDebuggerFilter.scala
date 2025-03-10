package org.clulab.odin.debugger.debug.filter

import org.clulab.odin.debugger.debug.context.ImmutableDebuggerContext
import org.clulab.odin.impl.{CrossSentenceExtractor, Extractor, GraphExtractor, TokenExtractor}
import org.clulab.processors.{Document, Sentence}

class DynamicDebuggerFilter(f: DynamicDebuggerFilter.FilterType) extends DebuggerFilter[DynamicDebuggerFilter.ArgumentType] {

  def apply(debuggerContext: ImmutableDebuggerContext): Boolean = f(debuggerContext)

  def sentenceFilter(sentence: Sentence): DynamicDebuggerFilter = {
    DynamicDebuggerFilter.and(this, DynamicDebuggerFilter.sentenceFilter(sentence))
  }

  def sentencesFilter(sentences: Seq[Sentence]): DynamicDebuggerFilter = {
    DynamicDebuggerFilter.and(this, DynamicDebuggerFilter.sentencesFilter(sentences))
  }

  def extractorFilter(extractor: Extractor): DynamicDebuggerFilter = {
    DynamicDebuggerFilter.and(this, DynamicDebuggerFilter.extractorFilter(extractor))
  }

  def extractorsFilter(extractors: Seq[Extractor]): DynamicDebuggerFilter = {
    DynamicDebuggerFilter.and(this, DynamicDebuggerFilter.extractorsFilter(extractors))
  }

  def and(other: DynamicDebuggerFilter): DynamicDebuggerFilter =
      DynamicDebuggerFilter.and(this, other)

  def or(other: DynamicDebuggerFilter): DynamicDebuggerFilter =
      DynamicDebuggerFilter.or(this, other)

  def not: DynamicDebuggerFilter = DynamicDebuggerFilter.not(this)
}

object DynamicDebuggerFilter {
  type ArgumentType = ImmutableDebuggerContext
  type FilterType = ArgumentType => Boolean

  val trueFilter: DynamicDebuggerFilter = {
    val f = (debuggerContext: ImmutableDebuggerContext) => {
      true
    }

    DynamicDebuggerFilter(f)
  }

  def apply(f: FilterType): DynamicDebuggerFilter = new DynamicDebuggerFilter(f)

  def and(left: DynamicDebuggerFilter, right: DynamicDebuggerFilter): DynamicDebuggerFilter = {
    val f = (debuggerContext: ArgumentType) => {
      left(debuggerContext) && right(debuggerContext)
    }

    DynamicDebuggerFilter(f)
  }

  def or(left: DynamicDebuggerFilter, right: DynamicDebuggerFilter): DynamicDebuggerFilter = {
    val f = (debuggerContext: ArgumentType) => {
      left(debuggerContext) || right(debuggerContext)
    }

    DynamicDebuggerFilter(f)
  }

  def not(filter: DynamicDebuggerFilter): DynamicDebuggerFilter = {
    val f = (debuggerContext: ArgumentType) => {
      !filter(debuggerContext)
    }

    DynamicDebuggerFilter(f)
  }

  def documentFilter(document: Document): DynamicDebuggerFilter = {
    val f = (debuggerContext: ArgumentType) => {
      debuggerContext.documentOpt.forall(document.eq)
    }

    DynamicDebuggerFilter(f)
  }

  def extractorFilter(extractor: Extractor): DynamicDebuggerFilter = {
    val f = (debuggerContext: ArgumentType) => {
      debuggerContext.extractorOpt.forall(extractor.eq)
    }

    DynamicDebuggerFilter(f)
  }

  def extractorsFilter(outerExtractors: Seq[Extractor]): DynamicDebuggerFilter = {
    val f = (debuggerContext: ArgumentType) => {
      debuggerContext.extractorOpt.forall { innerExtractor =>
        outerExtractors.exists { outerExtractor =>
          outerExtractor.eq(innerExtractor)
        }
      }
    }

    DynamicDebuggerFilter(f)
  }

  def sentenceFilter(sentence: Sentence): DynamicDebuggerFilter = {
    val f = (debuggerContext: ArgumentType) => {
      debuggerContext.sentenceOpt.forall(sentence.eq)
    }

    DynamicDebuggerFilter(f)
  }

  def sentencesFilter(outerSentences: Seq[Sentence]): DynamicDebuggerFilter = {
    val f = (debuggerContext: ArgumentType) => {
      debuggerContext.sentenceOpt.forall { innerSentence =>
        outerSentences.exists { outerSentence =>
          outerSentence.eq(innerSentence)
        }
      }
    }

    DynamicDebuggerFilter(f)
  }

  def multiFilter(document: Document, sentence: Sentence, extractor: Extractor): DynamicDebuggerFilter = {
    val f = (debuggerContext: ArgumentType) => {
      debuggerContext.documentOpt.forall(document.eq) &&
      debuggerContext.sentenceOpt.forall(sentence.eq) &&
      debuggerContext.extractorOpt.forall(extractor.eq)
    }

    DynamicDebuggerFilter(f)
  }

  def startTokFilter(start: Int, tok: Int): DynamicDebuggerFilter = {
    val f = (debuggerContext: ArgumentType) => {
      debuggerContext.startOpt.forall(_ == start) &&
      debuggerContext.tokOpt.forall(_ == tok)
    }

    DynamicDebuggerFilter(f)
  }


  def tokenExtractorFilter(extractor: TokenExtractor): DynamicDebuggerFilter = {
    extractorFilter(extractor)
  }

  def graphExtractorFilter(extractor: GraphExtractor): DynamicDebuggerFilter = {
    extractorFilter(extractor)
  }

  def crossSentenceExtractorFilter(extractor: CrossSentenceExtractor): DynamicDebuggerFilter = {
    val relatedExtractors = Seq(extractor, extractor.anchorPattern, extractor.neighborPattern)

    extractorsFilter(relatedExtractors)
  }
}
