package org.clulab.odin.debugger.odin

import org.clulab.odin.debugger.debug.{FinishedGlobalAction, FinishedInst, FinishedLocalAction, FinishedThread}
import org.clulab.odin.debugger.Debugger
import org.clulab.odin.impl._
import org.clulab.odin.{Action, ExtractorEngine, Mention, State}
import org.clulab.processors.Document

import scala.collection.mutable
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
    val debuggingExtractors = extractors.map(DebuggingExtractor(_, debugger))
    val globalAction = DebuggingAction(debugger, extractorEngine.globalAction, None)

    new InnerDebuggingExtractorEngine(debugger, debuggingExtractors, globalAction)
  }
}

class DebuggingExtractorEngine protected (extractors: Vector[Extractor], globalAction: Action, active: Boolean, verbose: Boolean)
    extends ExtractorEngine(extractors, globalAction) {
  val finishedInsts: mutable.Buffer[FinishedInst] = mutable.Buffer.empty
  val finishedThreads: mutable.Buffer[FinishedThread] = mutable.Buffer.empty
  val finishedLocalActions: mutable.Buffer[FinishedLocalAction] = mutable.Buffer.empty
  val finishedGlobalActions: mutable.Buffer[FinishedGlobalAction] = mutable.Buffer.empty

  def finish(debugger: Debugger): Unit = synchronized {
    finishedInsts.appendAll(debugger.instTranscript)
    finishedThreads.appendAll(debugger.threadTranscript)
    finishedLocalActions.appendAll(debugger.localActionTranscript)
    finishedGlobalActions.appendAll(debugger.globalActionTranscript)
  }

  override def extractFrom(doc: Document): Seq[Mention] = {
    val debugger = new Debugger(active, verbose)
    val inner = InnerDebuggingExtractorEngine(debugger, this)
    val result = inner.extractFrom(doc)

    finish(debugger)
    result
  }

  override def extractFrom(document: Document, initialState: State): Seq[Mention] = {
    val debugger = new Debugger(active, verbose)
    val inner = InnerDebuggingExtractorEngine(debugger, this)
    val result = inner.extractFrom(document, initialState)

    finish(debugger)
    result
  }

  override def extractByType[M <: Mention : ClassTag](document: Document, initialState: State): Seq[M] = {
    val debugger = new Debugger(active, verbose)
    val inner = InnerDebuggingExtractorEngine(debugger, this)
    val result = inner.extractByType[M](document, initialState)

    finish(debugger)
    result
  }

  def getExtractorByName(name: String): Extractor = {
    val extractor = extractors.find { extractor =>
      extractor.name == name
    }.get

    extractor
  }
}

object DebuggingExtractorEngine {

  def apply(extractorEngine: ExtractorEngine, active: Boolean = true, verbose: Boolean = false): DebuggingExtractorEngine = {
    val extractors = extractorEngine.extractors

    new DebuggingExtractorEngine(extractors, extractorEngine.globalAction, active, verbose)
  }
}
