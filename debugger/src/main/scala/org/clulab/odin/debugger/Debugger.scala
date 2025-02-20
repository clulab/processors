package org.clulab.odin.debugger

import org.clulab.odin.Mention
import org.clulab.odin.impl.ThompsonVM.{SingleThread, Thread}
import org.clulab.odin.impl.{Done, Extractor, Inst, TokenPattern}
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Interval
import org.clulab.utils.StringUtils

import scala.collection.mutable.Buffer

case class ThreadMatch(matches: Boolean, reason: String)

object ThreadMatch {
  val instMismatch = ThreadMatch(false, "Inst mismatch")
  val empty = ThreadMatch(false, "Thread empty")
  val lowerPriority = ThreadMatch(false, "Thread not the best")
  val superseded = ThreadMatch(false, "Thread superseded")
  val survivor = ThreadMatch(true, "Thread survived")
}

case class FinishedThread(
  thread: SingleThread,
  instMatch: Boolean,
  threadMatch: ThreadMatch,
  debuggerRecord: DebuggerRecord
)

case class FinishedInst(
  inst: Inst,
  instMatch: Boolean,
  debuggerRecord: DebuggerRecord
)

// Turn into SearchRecord, InstRecord, ThreadRecord
case class DebuggerRecord(
  // TODO: Maybe include depth?
  document: Document,
  loop: Int,
  extractor: Extractor,

  tokenPattern: TokenPattern,
  sentenceIndex: Int,
  sentence: Sentence,

  startOpt: Option[Int] = None,
  tokOpt: Option[Int] = None, // TODO: Maybe skip
  tokenIntervalOpt: Option[Interval] = None
)

class DebuggerContext(
  protected var depth: Int = 0,
  protected var documents: List[Document] = List.empty,
  protected var loops: List[Int] = List.empty,
  protected var extractors: List[Extractor] = List.empty,
  protected var tokenPatterns: List[TokenPattern] = List.empty,
  protected var sentenceIndexes: List[Int] = List.empty,
  protected var sentences: List[Sentence] = List.empty,
  protected var starts: List[Int] = List.empty, // Where in the sentence we are starting.
  protected var toks: List[Int] = List.empty, // The current token index
  protected var insts: List[Inst] = List.empty, // TODO: skip toks and insts because of Threads
  protected var tokenIntervals: List[Interval] = List.empty
) {
  def isComplete: Boolean = {
    documents.nonEmpty &&
    loops.nonEmpty &&
    extractors.nonEmpty &&
    tokenPatterns.nonEmpty &&
    sentenceIndexes.nonEmpty &&
    sentences.nonEmpty

    // tokenIntervals
  }

  def isInstComplete: Boolean = {
    isComplete && {
      starts.nonEmpty &&
      toks.nonEmpty //&&
//      insts.nonEmpty
    }
  }

  def isThreadComplete: Boolean = {
    isComplete && {
      starts.nonEmpty
    }
  }

  def getDepth: Int = depth

  def setDocument(document: Document): Unit = {
    documents = document :: documents
    depth += 1
  }

  def getDocumentOpt: Option[Document] = documents.headOption

  def resetDocument(): Unit = {
    documents = documents.tail
    depth -= 1
  }

  def setLoop(loop: Int): Unit = {
    loops = loop :: loops
    depth += 1
  }

  def getLoopOpt: Option[Int] = loops.headOption

  def resetLoop(): Unit = {
    loops = loops.tail
    depth -= 1
  }

  def setExtractor(extractor: Extractor): Unit = {
    extractors = extractor :: extractors
    depth += 1
  }

  def getExtractorOpt: Option[Extractor] = extractors.headOption

  def resetExtractor(): Unit = {
    extractors = extractors.tail
    depth -= 1
  }

  def setTokenPattern(tokenPattern: TokenPattern): Unit = {
    tokenPatterns = tokenPattern :: tokenPatterns
    depth += 1
  }

  def getTokenPattern: Option[TokenPattern] = tokenPatterns.headOption

  def resetTokenPattern(): Unit = {
    tokenPatterns = tokenPatterns.tail
    depth -= 1
  }

  def setSentence(sentenceIndex: Int, sentence: Sentence): Unit = {
    sentenceIndexes = sentenceIndex :: sentenceIndexes
    sentences = sentence :: sentences
    depth += 1
  }

  def getSentenceOpt: (Option[Int], Option[Sentence]) = (sentenceIndexes.headOption, sentences.headOption)

  def resetSentence(): Unit = {
    sentenceIndexes = sentenceIndexes.tail
    sentences = sentences.tail
    depth -= 1
  }

  def setStart(start: Int): Unit = {
    starts = start :: starts
    depth += 1
  }

  def getStartOpt: Option[Int] = starts.headOption

  def resetStart(): Unit = {
    starts = starts.tail
    depth -= 1
  }

  def setTok(tok: Int): Unit = {
    toks = tok :: toks
    depth += 1
  }

  def getTokOpt: (Option[Int]) = toks.headOption

  def getToksLength: Int = toks.length

  def resetTok(): Unit = {
    toks = toks.tail
    depth -= 1
  }

  def setTokenInterval(tokenInterval: Interval): Unit = {
    tokenIntervals = tokenInterval :: tokenIntervals
    depth += 1
  }

  def getTokenIntervalOpt: (Option[Interval]) = tokenIntervals.headOption

  def resetTokenInterval(): Unit = {
    tokenIntervals = tokenIntervals.tail
    depth -= 1
  }


  def setInst(inst: Inst): Unit = {
    insts = inst :: insts
    depth += 1
  }

  def getInstOpt: Option[Inst] = insts.headOption

  def getInstsLength: Int = insts.length

  def resetInst(): Unit = {
    insts = insts.tail
    depth -= 1
  }

  def mkDebuggerRecord(tok: Int): DebuggerRecord = {
    DebuggerRecord(
      documents.head,
      loops.head,
      extractors.head,
      tokenPatterns.head,
      sentenceIndexes.head,
      sentences.head,

      starts.headOption,
      Some(tok) // ,
      //      Some(inst),
      //      tokenIntervals.headOption,
      //      matches
    )
  }

  def setInstMatches(matches: Boolean, tok: Int, inst: Inst): FinishedInst = {

    if (!isInstComplete) {
      if (inst != Done)
        println("The record is not complete!")

    } // TODO: Depends on what kind of extractor.  How does one know?  Check most recent extractor?
    // Leave something else on the stack.


    if (tokenIntervals.nonEmpty) {
      // This is for cross
      val asExpected = starts.isEmpty && toks.isEmpty && insts.isEmpty
    }
    else {
      val asExpected = starts.nonEmpty && toks.nonEmpty && insts.nonEmpty
    }

    FinishedInst(inst, matches, mkDebuggerRecord(tok))
  }

  def setThreadMatches(thread: SingleThread, instMatches: Boolean, threadMatch: ThreadMatch): FinishedThread = {
    val debuggerRecord = mkDebuggerRecord(thread.tok)
    val finishedThread = FinishedThread(thread, instMatches, threadMatch, debuggerRecord)

    if (!isComplete)
      println("The record is not complete!")// TODO: Depends on what kind of extractor

    if (tokenIntervals.nonEmpty) {
      // This is for cross
      val asExpected = starts.isEmpty && toks.isEmpty && insts.isEmpty
    }
    else {
      val asExpected = starts.nonEmpty && toks.nonEmpty && insts.nonEmpty
    }

    finishedThread
  }
}

// TODO: There need to be different kinds of debuggers or at least contexts for different extractors.

class Debugger(var active: Boolean = true, verbose: Boolean = false) {
  protected var stack: Debugger.Stack = List()
  protected var maxDepth = 0
  protected var maxStack: Debugger.Stack = stack
  protected val context = new DebuggerContext()
  val instTranscript: Buffer[FinishedInst] = Buffer.empty
  val threadTranscript: Buffer[FinishedThread] = Buffer.empty

  def activate(): Unit = active = true

  def deactivate(): Unit = active = false

  def clear(): Unit = instTranscript.clear()

  protected def innerDebug[ResultType, StackFrameType <: StackFrame](stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    if (active) {
      stack = stackFrame :: stack
      if (stack.length > maxDepth)
        maxStack = stack

      val result = try {
        block
      }
      finally {
        stack = stack.tail
      }
      val execTime = stackFrame.stopTimer()
      //      println(s"Execution time for stack frame: $execTime nanoseconds")
      result
    }
    else {
      block
    }
  }

  protected def debugWithMessage[ResultType, StackFrameType <: StackFrame](mkMessage: (String) => String)
      (stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    if (active) {
      if (verbose) println(mkMessage("beg"))
      val result = innerDebug(stackFrame)(block)
      if (verbose) println(mkMessage("end"))
      result
    }
    else {
      block
    }
  }

  protected def innerDebugDoc[ResultType, StackFrameType <: StackFrame](doc: Document)
      (stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "doc"
      val where = stackFrame.sourceCode.toString
      val extractorEngineString = "[]"
      val docString = StringUtils.afterLast(doc.toString, '.')
      val textString = doc.text.getOrElse(doc.sentences.map { sentence => sentence.words.mkString(" ") }.mkString(" "))
      val message = s"""${tabs}${side} $what $where$extractorEngineString(doc = $docString("$textString"))"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setDocument(doc)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetDocument()

    result
  }

  protected def innerDebugLoop[ResultType, StackFrameType <: StackFrame](loop: Int)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "loop"
      val where = stackFrame.sourceCode.toString
      val extractorEngineString = "[]"
      val loopString = loop.toString
      val message = s"""${tabs}${side} $what $where$extractorEngineString(loop = $loopString)"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setLoop(loop)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetLoop()
    result
  }

  protected def innerDebugExtractor[ResultType, StackFrameType <: StackFrame](extractor: Extractor)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    // TODO: This could keep track of the mentions returned from the block and
    // do something special if there are none.

    // TODO: This could be part of a stack frame, no longer generic, like the TokenExtractorFrame.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "extractor"
      val where = stackFrame.sourceCode.toString
      val extractorString = s"""["${extractor.name}"]"""
      val message = s"""${tabs}${side} $what $where$extractorString()"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setExtractor(extractor)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetExtractor()
    result
  }

  protected def innerDebugAnchorExtractor[ResultType, StackFrameType <: StackFrame](extractor: Extractor)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    // TODO: This could keep track of the mentions returned from the block and
    // do something special if there are none.

    // TODO: This could be part of a stack frame, no longer generic, like the TokenExtractorFrame.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "anchorExtractor"
      val where = stackFrame.sourceCode.toString
      val extractorString = s"""["${extractor.name}"]"""
      val message = s"""${tabs}${side} $what $where$extractorString()"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setExtractor(extractor)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetExtractor()
    result
  }

  protected def innerDebugNeighborExtractor[ResultType, StackFrameType <: StackFrame](extractor: Extractor)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    // TODO: This could keep track of the mentions returned from the block and
    // do something special if there are none.

    // TODO: This could be part of a stack frame, no longer generic, like the TokenExtractorFrame.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "neighborExtractor"
      val where = stackFrame.sourceCode.toString
      val extractorString = s"""["${extractor.name}"]"""
      val message = s"""${tabs}${side} $what $extractorString$where()"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setExtractor(extractor)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetExtractor()
    result
  }

  protected def innerDebugTokenPattern[ResultType, StackFrameType <: StackFrame](tokenPattern: TokenPattern)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "tokenPattern"
      val where = stackFrame.sourceCode.toString
      val tokenPatternString = s"""["tokenPattern"]"""
      val message = s"""${tabs}${side} $what $where$tokenPatternString()"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setTokenPattern(tokenPattern)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetTokenPattern()
    result
  }

  protected def innerDebugTriggerTokenPattern[ResultType, StackFrameType <: StackFrame](tokenPattern: TokenPattern)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "triggerTokenPattern"
      val where = stackFrame.sourceCode.toString
      val tokenPatternString = s"""["tokenPattern"]"""
      val message = s"""${tabs}${side} $what $where$tokenPatternString()"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setTokenPattern(tokenPattern)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetTokenPattern()
    result
  }

  protected def innerDebugSentence[ResultType, StackFrameType <: StackFrame](index: Int, sentence: Sentence)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "sentence"
      val where = stackFrame.sourceCode.toString
      val extractorString = "[]"
      val sentenceString = sentence.words.mkString(" ")
      val message = s"""${tabs}${side} $what $where$extractorString(index = $index, sentence = "$sentenceString")"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setSentence(index, sentence)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetSentence()
    result
  }

  protected def innerDebugStart[ResultType, StackFrameType <: StackFrame](start: Int)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "start"
      val where = stackFrame.sourceCode.toString
      val tokenPatternString="[]"
      val message = s"""${tabs}${side} $what $where$tokenPatternString(start = $start)"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setStart(start)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetStart()
    result
  }

  protected def innerDebugTok[ResultType, StackFrameType <: StackFrame](tok: Int)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "tok"
      val where = stackFrame.sourceCode.toString
      val evaluatorString = "[]"
      val message = s"""${tabs}${side} $what $where$evaluatorString(tok = $tok)"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setTok(tok)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetTok()
    result
  }

  protected def innerDebugInst[ResultType, StackFrameType <: StackFrame](inst: Inst)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "inst"
      val where = stackFrame.sourceCode.toString
      val evaluatorString = "[]"
      val instString = inst.toString()
      val message = s"""${tabs}${side} $what $where$evaluatorString(inst = $instString)"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setInst(inst)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetInst()
    result
  }

  protected def innerDebugTokenInterval[ResultType, StackFrameType <: StackFrame](tokenInterval: Interval)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "tokenInterval"
      val where = stackFrame.sourceCode.toString
      val extractorString = "[]"
      val message = s"""${tabs}${side} $what $where$extractorString(tokenInterval = [${tokenInterval.start}, ${tokenInterval.end}))"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setTokenInterval(tokenInterval)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetTokenInterval()
    result
  }

  protected def innerDebugAction[StackFrameType <: StackFrame](inMentions: Seq[Mention], outMentions: Seq[Mention])(stackFrame: StackFrameType): Unit = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "tokenInterval"
      val where = stackFrame.sourceCode.toString
      val extractorString = "[]"
      val message = "TODO" // s"""${tabs}${side} $what $where$extractorString(tokenInterval = [${tokenInterval.start}, ${tokenInterval.end}))"""

      message
    }

    val message = mkMessage(context.getDepth) _

    // What does this involve?
    // Document, sentence, extractor, round?
    // What about global action?

    if (active) {
      if (verbose) println(message)
      // TODO
//      instTranscript += context.setInstMatches(matches, tok, inst)
    }
  }

  protected def innerDebugInstMatches[StackFrameType <: StackFrame](matches: Boolean, tok: Int, inst: Inst)(stackFrame: StackFrameType): Unit = {

    def mkMessage(depth: Int): String = {
      val tabs = "\t" * depth
      val what = "matches"
      val where = stackFrame.sourceCode.toString
      val evaluatorString = "[]"
      val message = s"""${tabs}$what $where$evaluatorString(matches = ${matches.toString})"""

      message
    }

    val message = mkMessage(context.getDepth)

    if (active) {
      if (verbose) println(message)
      instTranscript += context.setInstMatches(matches, tok, inst)
    }
  }

  protected def innerDebugThreadMatches[StackFrameType <: StackFrame](instMatches: Boolean, thread: SingleThread, threadMatch: ThreadMatch)(stackFrame: StackFrameType): Unit = {
    if (active) {
      threadTranscript += context.setThreadMatches(thread, instMatches, threadMatch)
    }
  }

  def showTrace(stack: Debugger.Stack): Unit = {
    stack.zipWithIndex.foreach { case (stackFrame, index) =>
      println(s"$index: $stackFrame")
    }
  }

  def showTrace(): Unit = showTrace(stack)

  def showDeepest(): Unit = showTrace(maxStack)

  def debug[ResultType, StackFrameType <: StackFrame](stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    innerDebug(stackFrame)(block)
  }

  def debug[ResultType](block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    debug(stackFrame)(block)
  }

  // The extractorEngine needs to come in with the doc so that client code does not need to call debug
  // methods on the extractorEngine itself.
  def debugDoc[ResultType](doc: Document)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    innerDebugDoc(doc)(stackFrame)(block)
  }

  def debugLoop[ResultType](loop: Int)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    innerDebugLoop(loop)(stackFrame)(block)
  }

  def debugExtractor[ResultType](extractor: Extractor)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    innerDebugExtractor(extractor)(stackFrame)(block)
  }

  // TODO: We should know the document already, so the index should suffice.
  def debugSentence[ResultType](index: Int, sentence: Sentence)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    innerDebugSentence(index, sentence)(stackFrame)(block)
  }

  def debugStart[ResultType](start: Int)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    innerDebugStart(start)(stackFrame)(block)
  }

  def debugTok[ResultType](tok: Int)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    innerDebugTok(tok)(stackFrame)(block)
  }

  def debugInst[ResultType](inst: Inst)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    innerDebugInst(inst)(stackFrame)(block)
  }

  def debugInstMatches(matches: Boolean, tok: Int, inst: Inst)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): Unit = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    innerDebugInstMatches(matches, tok, inst)(stackFrame)
  }

  def debugThreadMatches(thread: SingleThread, matches: Boolean, threadMatch: ThreadMatch)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): Unit = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    innerDebugThreadMatches(matches, thread, threadMatch)(stackFrame)
  }

  def debugAnchor[ResultType](extractor: Extractor)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    // TODO: Record some kind of cross type "anchor"
    innerDebugAnchorExtractor(extractor)(stackFrame)(block)
  }

  def debugNeighbor[ResultType](extractor: Extractor)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    // TODO: Record some kind of cross type "neighbor"
    innerDebugNeighborExtractor(extractor)(stackFrame)(block)
  }

  def debugTokenPattern[ResultType](tokenPattern: TokenPattern)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    innerDebugTokenPattern(tokenPattern)(stackFrame)(block)
  }

  // This skips the TokenExtractor and goes straight for the TokenPattern
  def debugTrigger[ResultType](tokenPattern: TokenPattern)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    innerDebugTriggerTokenPattern(tokenPattern)(stackFrame)(block)
  }

  def debugTokenInterval[ResultType](tokenInterval: Interval)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    innerDebugTokenInterval(tokenInterval)(stackFrame)(block)
  }

  def debugAction(inMentions: Seq[Mention], outMentions: Seq[Mention])(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): Unit = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    innerDebugAction(inMentions, outMentions)(stackFrame)
  }
}

object Debugger {
  type Stack = List[StackFrame]
}
