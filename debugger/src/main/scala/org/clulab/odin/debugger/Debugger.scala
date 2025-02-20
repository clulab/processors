package org.clulab.odin.debugger

import org.clulab.odin.Mention
import org.clulab.odin.debugger.debug.{DebuggerContext, FinishedInst, FinishedThread, SourceCode, StackFrame, ThreadMatch}
import org.clulab.odin.impl.ThompsonVM.{SingleThread, Thread}
import org.clulab.odin.impl.{Done, Extractor, Inst, TokenPattern}
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Interval
import org.clulab.utils.StringUtils

import scala.collection.mutable.Buffer

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
      val what = "action"
      val where = stackFrame.sourceCode.toString
      val extractorString = "[]"
      val message = s"""${tabs}${side} $what $where$extractorString"""

      message
    }

    val message = mkMessage(context.getDepth) _

    if (active) {
      if (verbose) println(message)
//      actionTranscript += context.setInstMatches(inMentions, outMentions)
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
