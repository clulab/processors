package org.clulab.odin.debugger.odin

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.debugger.debug.filter.{DynamicDebuggerFilter, StaticDebuggerFilter}
import org.clulab.odin.debugger.debug.finished.{FinishedGlobalAction, FinishedInst, FinishedLocalAction, FinishedMention, FinishedThread}
import org.clulab.odin.debugger.utils.Transcript
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

class DebuggingExtractorEngine protected (
  extractors: Vector[Extractor],
  globalAction: Action,
  val dynamicDebuggerFilter: DynamicDebuggerFilter,
  val staticDebuggerFilter: StaticDebuggerFilter,
  active: Boolean,
  verbose: Boolean
) extends ExtractorEngine(extractors, globalAction) {
  val finishedInsts  = Transcript[FinishedInst]()
  val finishedThreads = Transcript[FinishedThread]()
  val finishedLocalActions = Transcript[FinishedLocalAction]()
  val finishedGlobalActions = Transcript[FinishedGlobalAction]()
  val finishedMentions = Transcript[FinishedMention]()

  def finish(debugger: Debugger): Unit = synchronized {
    finishedInsts.appendAll(debugger.instTranscript)
    finishedThreads.appendAll(debugger.threadTranscript)
    finishedLocalActions.appendAll(debugger.localActionTranscript)
    finishedGlobalActions.appendAll(debugger.globalActionTranscript)
    finishedMentions.appendAll(debugger.mentionTranscript)
  }

  override def extractFrom(doc: Document): Seq[Mention] = {
    val debugger = new Debugger(dynamicDebuggerFilter, active, verbose)
    val inner = InnerDebuggingExtractorEngine(debugger, this)
    val result = inner.extractFrom(doc)

    finish(debugger)
    result
  }

  override def extractFrom(document: Document, initialState: State): Seq[Mention] = {
    val debugger = new Debugger(dynamicDebuggerFilter, active, verbose)
    val inner = InnerDebuggingExtractorEngine(debugger, this)
    val result = inner.extractFrom(document, initialState)

    finish(debugger)
    result
  }

  override def extractByType[M <: Mention : ClassTag](document: Document, initialState: State): Seq[M] = {
    val debugger = new Debugger(dynamicDebuggerFilter, active, verbose)
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

  def apply(
    extractorEngine: ExtractorEngine,
    dynamicDebuggerFilter: DynamicDebuggerFilter = DynamicDebuggerFilter.trueFilter,
    staticDebuggerFilter: StaticDebuggerFilter = StaticDebuggerFilter.trueFilter,
    active: Boolean = true,
    verbose: Boolean = false
  ): DebuggingExtractorEngine = {
    val extractors = extractorEngine.extractors

    new DebuggingExtractorEngine(extractors, extractorEngine.globalAction, dynamicDebuggerFilter,
        staticDebuggerFilter, active, verbose)
  }

  def getExtractorByNameOpt(extractorEngine: ExtractorEngine, name: String): Option[Extractor] = {
    extractorEngine.extractors.find(_.name == name)
  }

  def getExtractorByName(extractorEngine: ExtractorEngine, name: String): Extractor = {
    getExtractorByNameOpt(extractorEngine, name).get
  }
}
