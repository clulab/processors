package org.clulab.odin.debugger.debugging

import org.clulab.odin.debugger.{Debugger, LocalDebugger}
import org.clulab.odin.impl._
import org.clulab.odin.{Action, ExtractorEngine, Mention, State}
import org.clulab.processors.Document

import scala.reflect.ClassTag

class InnerDebuggingExtractorEngine(val debugger: Debugger, extractors: Vector[Extractor], globalAction: Action)
    extends ExtractorEngine(extractors, globalAction) {

  override protected def extract(document: Document, i: Int, state: State): Seq[Mention] = debugger.debugLoop(i) {
    super.extract(document, i, state)
  }

  override def extractFrom(document: Document, initialState: State): Seq[Mention] = debugger.debugDoc(document) {
    super.extractFrom(document, initialState)
  }
}

object InnerDebuggingExtractorEngine {

  def apply(debugger: Debugger, extractorEngine: ExtractorEngine): InnerDebuggingExtractorEngine = {
    val extractors = extractorEngine.extractors
    val debuggingExtractors = extractors.map {
      case extractor: TokenExtractor => DebuggingTokenExtractor(debugger, extractor)
      case extractor: GraphExtractor => DebuggingGraphExtractor(debugger, extractor)
      case extractor: CrossSentenceExtractor => DebuggingCrossSentenceExtractor(debugger, extractor)
    }
    val globalAction = extractorEngine.globalAction

    new InnerDebuggingExtractorEngine(debugger, debuggingExtractors, globalAction)
  }
}

class DebuggingExtractorEngine protected (extractors: Vector[Extractor], globalAction: Action)
    extends ExtractorEngine(extractors, globalAction) {

  // We do have a list of extractors here and they are indexed.
  // Maybe in the log we need to store the index rather than extractor itself.
  // The debugging extractor needs to have an index then.
  // What if an extractor is present twice?  getIndex could return multiple values
  // and the user would have to decide?
  // Get records for particular extractor
  // By sentence
  // Get the trace and get any extractor details
  // Send those to the debugger
  // Call this Tracer rather than Debugger?
  // May depend on what all it needs do to.

  override def extractFrom(doc: Document): Seq[Mention] = {
    val debugger =  new LocalDebugger()
    val inner = InnerDebuggingExtractorEngine(debugger, this)
    val result = inner.extractFrom(doc)
    // Get stuff out of the debugger
    // Save it in a synchronized way to the transcript

    result
  }

  override def extractFrom(document: Document, initialState: State): Seq[Mention] = {
    val debugger = new LocalDebugger()
    val inner = InnerDebuggingExtractorEngine(debugger, this)
    val result = inner.extractFrom(document, initialState)
    // Get stuff out of the debugger
    // Save it in a synchronized way to the transcript

    result
  }

  override def extractByType[M <: Mention : ClassTag](document: Document, initialState: State): Seq[M] = {
    val debugger = new LocalDebugger()
    val inner = InnerDebuggingExtractorEngine(debugger, this)
    val result = inner.extractByType[M](document, initialState)
    // Get stuff out of the debugger
    // Save it in a synchronized way to the transcript

    result
  }

  // Need extra stuff to fish out the right extractor
  // Need to get out the right extractor, how?
  // Maybe get out the right inst of it by number?
  // Then need document and sentence.
  // Write various kinds of queries.
}

object DebuggingExtractorEngine {

  def apply(extractorEngine: ExtractorEngine): DebuggingExtractorEngine = {
    val extractors = extractorEngine.extractors
    val debuggingExtractors = extractors.map { extractor =>
      extractor match {
        case extractor: TokenExtractor => ??? // Make one with en empty debugger spot to be filled in when another copy made
        case extractor: GraphExtractor => ???
        case extractor: CrossSentenceExtractor => ???
      }
    }
    val globalAction = extractorEngine.globalAction

    new DebuggingExtractorEngine(debuggingExtractors, globalAction)

  }
}