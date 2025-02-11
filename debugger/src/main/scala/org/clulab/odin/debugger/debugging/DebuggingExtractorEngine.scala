package org.clulab.odin.debugger.debugging

import org.clulab.odin.debugger.{Debugger, FinishedInst, FinishedThread}
import org.clulab.odin.impl._
import org.clulab.odin.{Action, ExtractorEngine, Mention, State}
import org.clulab.processors.Document

import scala.collection.mutable.Buffer
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
    // TODO: change back to debugging version
    val debuggingExtractors = extractors.map {
      case extractor: TokenExtractor => DebuggingTokenExtractor(debugger, extractor)
      case extractor: GraphExtractor => DebuggingGraphExtractor(debugger, extractor)
      case extractor: CrossSentenceExtractor => DebuggingCrossSentenceExtractor(debugger, extractor)
    }
    val globalAction = extractorEngine.globalAction

    new InnerDebuggingExtractorEngine(debugger, debuggingExtractors, globalAction)
  }
}

class DebuggingExtractorEngine protected (extractors: Vector[Extractor], globalAction: Action, verbose: Boolean)
    extends ExtractorEngine(extractors, globalAction) {
  val transcript: Buffer[FinishedInst] = Buffer.empty
  val finishedThreads: Buffer[FinishedThread] = Buffer.empty

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

  def extractTranscript(debugger: Debugger): Unit = synchronized {
    transcript.appendAll(debugger.instTranscript)
  }

  def extractFinishedThreads(debugger: Debugger): Unit = synchronized {
    finishedThreads.appendAll(debugger.threadTranscript)
  }

  override def extractFrom(doc: Document): Seq[Mention] = {
    val debugger = new Debugger(verbose)
    val inner = InnerDebuggingExtractorEngine(debugger, this)
    val result = inner.extractFrom(doc)

    extractTranscript(debugger)
    extractFinishedThreads(debugger)
    result
  }

  override def extractFrom(document: Document, initialState: State): Seq[Mention] = {
    val debugger = new Debugger(verbose)
    val inner = InnerDebuggingExtractorEngine(debugger, this)
    val result = inner.extractFrom(document, initialState)

    extractTranscript(debugger)
    extractFinishedThreads(debugger)
    result
  }

  override def extractByType[M <: Mention : ClassTag](document: Document, initialState: State): Seq[M] = {
    val debugger = new Debugger(verbose)
    val inner = InnerDebuggingExtractorEngine(debugger, this)
    val result = inner.extractByType[M](document, initialState)

    extractTranscript(debugger)
    extractFinishedThreads(debugger)
    result
  }

  def getExtractorByName(name: String): Extractor = {
    val extractor = extractors.find { extractor =>
      extractor.name == "person-from-lexicon"
    }.get

    extractor
  }
}

object DebuggingExtractorEngine {

  def apply(extractorEngine: ExtractorEngine, verbose: Boolean = false): DebuggingExtractorEngine = {
    val extractors = extractorEngine.extractors
    val globalAction = extractorEngine.globalAction

    new DebuggingExtractorEngine(extractors, globalAction, verbose)
  }
}
