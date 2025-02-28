package org.clulab.odin.debugger

import org.clulab.odin.Mention
import org.clulab.odin.debugger.debug.context.MutableDebuggerContext
import org.clulab.odin.debugger.debug.filter.DynamicDebuggerFilter
import org.clulab.odin.debugger.debug.finished.{FinishedGlobalAction, FinishedInst, FinishedLocalAction, FinishedMention, FinishedThread}
import org.clulab.odin.debugger.debug.{MentionMatch, SourceCode, StackFrame, ThreadMatch, Transcript}
import org.clulab.odin.impl.ThompsonVM.SingleThread
import org.clulab.odin.impl.{Extractor, Inst, TokenPattern}
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Interval
import org.clulab.utils.StringUtils

class Debugger(val filter: DynamicDebuggerFilter, var active: Boolean = true, verbose: Boolean = false) {
  protected var stack: Debugger.Stack = List()
  protected var maxDepth = 0
  protected var maxStack: Debugger.Stack = stack
  protected val context = new MutableDebuggerContext(filter)

  val instTranscript = Transcript[FinishedInst]()
  val threadTranscript = Transcript[FinishedThread]()
  val localActionTranscript = Transcript[FinishedLocalAction]()
  val globalActionTranscript = Transcript[FinishedGlobalAction]()
  val mentionTranscript = Transcript[FinishedMention]()

  val transcripts: Seq[Transcript[_]] = Seq(
    instTranscript,
    threadTranscript,
    localActionTranscript,
    globalActionTranscript,
    mentionTranscript
  )

  def activate(): Unit = active = true

  def deactivate(): Unit = active = false

  def clear(): Unit = transcripts.foreach(_.clear)

  protected def innerDebug[ResultType, StackFrameType <: StackFrame](stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    // Active should have been handled by now.
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

  protected def innerDebugWithMessage[ResultType, StackFrameType <: StackFrame](mkMessage: (String) => String)
      (stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    // Active should have been handled by now.
    if (verbose) println(mkMessage("beg"))
    val result = innerDebug(stackFrame)(block)
    if (verbose) println(mkMessage("end"))
    result
  }

  protected def innerDebugDoc[ResultType, StackFrameType <: StackFrame](doc: Document)
      (stackFrame: StackFrameType)(block: => ResultType): ResultType = {

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
    val result = context.withDocument(doc) { innerDebugWithMessage(message)(stackFrame)(block) }

    result
  }

  protected def innerDebugLoop[ResultType, StackFrameType <: StackFrame](loop: Int)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

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
    val result = context.withLoop(loop) { innerDebugWithMessage(message)(stackFrame)(block) }

    result
  }

  protected def innerDebugExtractor[ResultType, StackFrameType <: StackFrame](extractor: Extractor)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "extractor"
      val where = stackFrame.sourceCode.toString
      val extractorString = s"""["${extractor.name}"]"""
      val message = s"""${tabs}${side} $what $where$extractorString()"""

      message
    }

    val message = mkMessage(context.getDepth) _
    val result = context.withExtractor(extractor) { innerDebugWithMessage(message)(stackFrame)(block) }

    result
  }

  protected def innerDebugAnchorExtractor[ResultType, StackFrameType <: StackFrame](extractor: Extractor)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

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
    val result = context.withExtractor(extractor) { innerDebugWithMessage(message)(stackFrame)(block) }

    result
  }

  protected def innerDebugNeighborExtractor[ResultType, StackFrameType <: StackFrame](extractor: Extractor)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "neighborExtractor"
      val where = stackFrame.sourceCode.toString
      val extractorString = s"""["${extractor.name}"]"""
      val message = s"""${tabs}${side} $what $extractorString$where()"""

      message
    }

    val message = mkMessage(context.getDepth) _
    val result = context.withExtractor(extractor) { innerDebugWithMessage(message)(stackFrame)(block) }

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
    val result = context.withTokenPattern(tokenPattern) { innerDebugWithMessage(message)(stackFrame)(block) }

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
    val result = context.withTokenPattern(tokenPattern) { innerDebugWithMessage(message)(stackFrame)(block) }

    result
  }

  protected def innerDebugSentence[ResultType, StackFrameType <: StackFrame](index: Int, sentence: Sentence)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

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
    val result = context.withSentence(index, sentence) { innerDebugWithMessage(message)(stackFrame)(block) }

    result
  }

  protected def innerDebugStart[ResultType, StackFrameType <: StackFrame](start: Int)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "start"
      val where = stackFrame.sourceCode.toString
      val tokenPatternString="[]"
      val message = s"""${tabs}${side} $what $where$tokenPatternString(start = $start)"""

      message
    }

    val message = mkMessage(context.getDepth) _
    val result = context.withStart(start) { innerDebugWithMessage(message)(stackFrame)(block) }

    result
  }

  protected def innerDebugTok[ResultType, StackFrameType <: StackFrame](tok: Int)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "tok"
      val where = stackFrame.sourceCode.toString
      val evaluatorString = "[]"
      val message = s"""${tabs}${side} $what $where$evaluatorString(tok = $tok)"""

      message
    }

    val message = mkMessage(context.getDepth) _
    val result = context.withTok(tok) { innerDebugWithMessage(message)(stackFrame)(block) }

    result
  }

  protected def innerDebugTokenInterval[ResultType, StackFrameType <: StackFrame](tokenInterval: Interval)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "tokenInterval"
      val where = stackFrame.sourceCode.toString
      val extractorString = "[]"
      val message = s"""${tabs}${side} $what $where$extractorString(tokenInterval = [${tokenInterval.start}, ${tokenInterval.end}))"""

      message
    }

    val message = mkMessage(context.getDepth) _
    val result = context.withTokenInterval(tokenInterval) { innerDebugWithMessage(message)(stackFrame)(block) }

    result
  }

  protected def innerDebugInstMatches[StackFrameType <: StackFrame](matches: Boolean, tok: Int, inst: Inst)(stackFrame: StackFrameType): Unit = {

    def mkMessage(depth: Int): String = {
      val tabs = "\t" * depth
      val what = "instMatches"
      val where = stackFrame.sourceCode.toString
      val evaluatorString = "[]"
      val message = s"""${tabs}$what $where$evaluatorString(matches = ${matches.toString}, ...)"""

      message
    }

    val message = mkMessage(context.getDepth)

    if (verbose) println(message)
    instTranscript.appendOpt(context.setInstMatches(matches, tok, inst))
  }

  protected def innerDebugThreadMatches[StackFrameType <: StackFrame](instMatches: Boolean, thread: SingleThread, threadMatch: ThreadMatch)(stackFrame: StackFrameType): Unit = {
    innerDebugInstMatches(instMatches, thread.tok, thread.inst)(stackFrame)

    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "threadMatches"
      val where = stackFrame.sourceCode.toString
      val extractorString = "[]"
      val message = s"""${tabs}${side} $what $where$extractorString(...)"""

      message
    }

    val message = mkMessage(context.getDepth) _

    if (verbose) println(message)
    threadTranscript.appendOpt(context.setThreadMatches(thread, threadMatch))
  }

  protected def innerDebugMentionMatches[StackFrameType <: StackFrame](mention: Mention, stateMentions: Seq[Mention], mentionMatches: Seq[MentionMatch])(stackFrame: StackFrameType): Unit = {

    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = "mentionMatches"
      val where = stackFrame.sourceCode.toString
      val extractorString = "[]"
      val message = s"""${tabs}${side} $what $where$extractorString(...)"""

      message
    }

    val message = mkMessage(context.getDepth) _

    if (verbose) println(message)
    mentionTranscript.appendOpt(context.setMentionMatches(mention, stateMentions, mentionMatches))
  }


  protected def innerDebugActionMatches[StackFrameType <: StackFrame](inMentions: Seq[Mention], outMentions: Seq[Mention])(stackFrame: StackFrameType): Unit = {
    val isLocal = context.getExtractorOpt.nonEmpty

    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val what = if (isLocal) "localActionMatches" else "globalActionMatches"
      val where = stackFrame.sourceCode.toString
      val extractorString = "[]"
      val message = s"""${tabs}${side} $what $where$extractorString(...)"""

      message
    }

    val message = mkMessage(context.getDepth) _

    if (verbose) println(message)
    if (isLocal)
      localActionTranscript.appendOpt(context.setLocalActionMatches(inMentions, outMentions))
    else
      globalActionTranscript.appendOpt(context.setGlobalActionMatches(inMentions, outMentions))
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
    if (active) {
      val sourceCode = new SourceCode(line, fileName, enclosing)
      val stackFrame = new StackFrame(sourceCode)

      context.withDocument(doc) { innerDebugDoc(doc)(stackFrame)(block) }
    }
    else block
  }

  def debugLoop[ResultType](loop: Int)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    if (active) {
      val sourceCode = new SourceCode(line, fileName, enclosing)
      val stackFrame = new StackFrame(sourceCode)

      innerDebugLoop(loop)(stackFrame)(block)
    }
    else block
  }

  def debugExtractor[ResultType](extractor: Extractor)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    if (active) {
      val sourceCode = new SourceCode(line, fileName, enclosing)
      val stackFrame = new StackFrame(sourceCode)

      innerDebugExtractor(extractor)(stackFrame)(block)
    }
    else block
  }

  // TODO: We should know the document already, so the index should suffice.
  def debugSentence[ResultType](index: Int, sentence: Sentence)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    if (active) {
      val sourceCode = new SourceCode(line, fileName, enclosing)
      val stackFrame = new StackFrame(sourceCode)

      innerDebugSentence(index, sentence)(stackFrame)(block)
    }
    else block
  }

  def debugStart[ResultType](start: Int)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    if (active) {
      val sourceCode = new SourceCode(line, fileName, enclosing)
      val stackFrame = new StackFrame(sourceCode)

      innerDebugStart(start)(stackFrame)(block)
    }
    else block
  }

  def debugTok[ResultType](tok: Int)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    if (active) {
      val sourceCode = new SourceCode(line, fileName, enclosing)
      val stackFrame = new StackFrame(sourceCode)

      innerDebugTok(tok)(stackFrame)(block)
    }
    else block
  }

  def debugInstMatches(matches: Boolean, tok: Int, inst: Inst)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): Unit = {
    if (active) {
      val sourceCode = new SourceCode(line, fileName, enclosing)
      val stackFrame = new StackFrame(sourceCode)

      innerDebugInstMatches(matches, tok, inst)(stackFrame)
    }
  }

  def debugThreadMatches(thread: SingleThread, matches: Boolean, threadMatch: ThreadMatch)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): Unit = {
    if (active) {
      val sourceCode = new SourceCode(line, fileName, enclosing)
      val stackFrame = new StackFrame(sourceCode)

      innerDebugThreadMatches(matches, thread, threadMatch)(stackFrame)
    }
  }

  def debugMentionMatches(mention: Mention, stateMentions: Seq[Mention], mentionMatches: Seq[MentionMatch])(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): Unit = {
    if (active) {
      val sourceCode = new SourceCode(line, fileName, enclosing)
      val stackFrame = new StackFrame(sourceCode)

      innerDebugMentionMatches(mention, stateMentions, mentionMatches)(stackFrame)
    }
  }

  def debugAnchor[ResultType](extractor: Extractor)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    if (active) {
      val sourceCode = new SourceCode(line, fileName, enclosing)
      val stackFrame = new StackFrame(sourceCode)

      // TODO: Record some kind of cross type "anchor"
      innerDebugAnchorExtractor(extractor)(stackFrame)(block)
    }
    else block
  }

  def debugNeighbor[ResultType](extractor: Extractor)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    if (active) {
      val sourceCode = new SourceCode(line, fileName, enclosing)
      val stackFrame = new StackFrame(sourceCode)

      // TODO: Record some kind of cross type "neighbor"
      innerDebugNeighborExtractor(extractor)(stackFrame)(block)
    }
    else block
  }

  def debugTokenPattern[ResultType](tokenPattern: TokenPattern)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    if (active) {
      val sourceCode = new SourceCode(line, fileName, enclosing)
      val stackFrame = new StackFrame(sourceCode)

      innerDebugTokenPattern(tokenPattern)(stackFrame)(block)
    }
    else block
  }

  // This skips the TokenExtractor and goes straight for the TokenPattern
  def debugTrigger[ResultType](tokenPattern: TokenPattern)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    if (active) {
      val sourceCode = new SourceCode(line, fileName, enclosing)
      val stackFrame = new StackFrame(sourceCode)

      innerDebugTriggerTokenPattern(tokenPattern)(stackFrame)(block)
    }
    else block
  }

  def debugTokenInterval[ResultType](tokenInterval: Interval)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    if (active) {
      val sourceCode = new SourceCode(line, fileName, enclosing)
      val stackFrame = new StackFrame(sourceCode)

      innerDebugTokenInterval(tokenInterval)(stackFrame)(block)
    }
    else block
  }

  def debugActionMatches(inMentions: Seq[Mention], outMentions: Seq[Mention])(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): Unit = {
    if (active) {
      val sourceCode = new SourceCode(line, fileName, enclosing)
      val stackFrame = new StackFrame(sourceCode)

      innerDebugActionMatches(inMentions, outMentions)(stackFrame)
    }
  }
}

object Debugger {
  type Stack = List[StackFrame]
}
