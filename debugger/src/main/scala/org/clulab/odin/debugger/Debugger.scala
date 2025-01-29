package org.clulab.odin.debugger

import org.clulab.odin.impl.{Extractor, Inst}
import org.clulab.processors.{Document, Sentence}
import org.clulab.utils.StringUtils

import scala.collection.mutable.Buffer

case class DebuggerRecord(
  document: Document,
  loop: Int,
  extractor: Extractor,
  sentenceIndex: Int,
  sentence: Sentence,
  start: Int,
  tok: Int,
  inst: Inst,
  matches: Boolean
)

class DebuggerContext(
  protected var depth: Int = 0,
  protected var documentOpt: Option[Document] = None,
  protected var loopOpt: Option[Int] = None,
  protected var extractorOpt: Option[Extractor] = None,
  protected var sentenceIndexOpt: Option[Int] = None,
  protected var sentenceOpt: Option[Sentence] = None,
  protected var startOpt: Option[Int] = None,
  protected var toks: List[Int] = List.empty,
  protected var insts: List[Inst] = List.empty
) {
  def isComplete: Boolean = {
    documentOpt.isDefined &&
    loopOpt.isDefined &&
    extractorOpt.isDefined &&
    sentenceIndexOpt.isDefined &&
    sentenceOpt.isDefined &&
    startOpt.isDefined &&
    toks.nonEmpty &&
    insts.nonEmpty
  }

  def getDepth: Int = depth

  def setDocument(document: Document): Unit = {
    assert(documentOpt.isEmpty)
    documentOpt = Some(document)
    depth += 1
  }

  def getDocumentOpt: Option[Document] = documentOpt

  def resetDocument(): Unit = {
    documentOpt = None
    depth -= 1
  }

  def setLoop(loop: Int): Unit = {
    assert(documentOpt.nonEmpty)
    assert(loopOpt.isEmpty)
    loopOpt = Some(loop)
    depth += 1
  }

  def getLoopOpt: Option[Int] = loopOpt

  def resetLoop(): Unit = {
    loopOpt = None
    depth -= 1
  }

  def setExtractor(extractor: Extractor): Unit = {
    assert(loopOpt.nonEmpty)
    assert(extractorOpt.isEmpty)
    extractorOpt = Some(extractor)
    depth += 1
  }

  def getExtractorOpt: Option[Extractor] = extractorOpt

  def resetExtractor(): Unit = {
    extractorOpt = None
    depth -= 1
  }

  def setSentence(sentenceIndex: Int, sentence: Sentence): Unit = {
    assert(loopOpt.nonEmpty)
    assert(sentenceIndexOpt.isEmpty)
    assert(sentenceOpt.isEmpty)
    sentenceIndexOpt = Some(sentenceIndex)
    sentenceOpt = Some(sentence)
    depth += 1
  }

  def getSentenceOpt: (Option[Int], Option[Sentence]) = (sentenceIndexOpt, sentenceOpt)

  def resetSentence(): Unit = {
    sentenceIndexOpt = None
    sentenceOpt = None
    depth -= 1
  }

  def setStart(start: Int): Unit = {
    assert(sentenceIndexOpt.nonEmpty)
    assert(sentenceOpt.nonEmpty)
    assert(startOpt.isEmpty)
    assert(toks.isEmpty)
    assert(insts.isEmpty)
    startOpt = Some(start)
    depth += 1
  }

  def getStartOpt: Option[Int] = startOpt

  def resetStart(): Unit = {
    startOpt = None
    depth -= 1
  }

  def setTok(tok: Int): Unit = {
    // If matches and go to next token, just in case there is a pass, then still have last tok
    // can this be
    assert(startOpt.nonEmpty)
    toks = tok :: toks
//    assert(insts.isEmpty)
    depth += 1
  }

  // TODO: Need to take recursion amount into account
  // That should just be counted and put into context.

  def getTokOpt: (Option[Int]) = toks.headOption

  def getToksLength: Int = toks.length

  def resetTok(): Unit = {
    toks = toks.tail
    depth -= 1
  }

  def setInst(inst: Inst): Unit = {
    if (toks.isEmpty)
      println("How?")
//    assert(tokOpt.nonEmpty)
    // There can be multiple inst before we return.
    insts = inst :: insts
    depth += 1
  }

  def getInstOpt: Option[Inst] = insts.headOption

  def getInstsLength: Int = insts.length

  def resetInst(): Unit = {
    insts = insts.tail
    depth -= 1
  }

  def setMatches(matches: Boolean): DebuggerRecord = {
//    assert(isComplete)
    DebuggerRecord(documentOpt.get, loopOpt.get, extractorOpt.get, sentenceIndexOpt.get, sentenceOpt.get,
      startOpt.get, toks.head, insts.head, matches)
  }
}

class Debugger() {
  protected var active: Boolean = true // TODO: You can turn off debugging with this!
  protected var quiet: Boolean = false // TODO: You can turn off the printing of messages with this.
  protected var stack: Debugger.Stack = List()
  protected var maxDepth = 0
  protected var maxStack: Debugger.Stack = stack
  protected val context = new DebuggerContext()
  val transcript: Buffer[DebuggerRecord] = Buffer.empty

  def activate(): Unit = active = true

  def deactivate(): Unit = active = false

  def clear(): Unit = transcript.clear()

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
      if (!quiet) println(mkMessage("beg"))
      val result = innerDebug(stackFrame)(block)
      if (!quiet) println(mkMessage("end"))
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
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
          .replace("#", s"$extractorString.")
      val docString = StringUtils.afterLast(doc.toString, '.')
      val textString = doc.text.getOrElse(doc.sentences.map { sentence => sentence.words.mkString(" ") }.mkString(" "))
      val message = s"""${tabs}${side} $where(doc = $docString("$textString"))"""

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
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
          .replace("#", s"$extractorString.")
          .replace(' ', '.')
      val loopString = loop.toString
      val message = s"""${tabs}${side} $where(loop = $loopString)"""

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
      val extractorString = s"""["${extractor.name}"]"""
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
        .replace("#", s"$extractorString.")
      val message = s"""${tabs}${side} $where()"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setExtractor(extractor)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetExtractor()
    result
  }

  protected def innerDebugSentence[ResultType, StackFrameType <: StackFrame](index: Int, sentence: Sentence)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(depth: Int)(side: String): String = {
      val tabs = "\t" * depth
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
        .replace("#", s"$extractorString.")
        .replace(' ', '.')
      val sentenceString = sentence.words.mkString(" ")
      val message = s"""${tabs}${side} $where(index = $index, sentence = "$sentenceString")"""

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
      val method = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
      val obj = StringUtils.afterLast(StringUtils.beforeLast(stackFrame.sourceCode.enclosing.value, '.'), '.')
      val message = s"""${tabs}${side} $obj.$method(start = $start)"""

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
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
        .replace("#", s"$extractorString.")
        .replace(' ', '.')
      val message = s"""${tabs}${side} $where(tok = $tok)"""

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
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
          .replace("#", s"$extractorString.")
          .replace(' ', '.')
      val instString = inst.toString
      val message = s"""${tabs}${side} $where(inst = $instString)"""

      message
    }

    val message = mkMessage(context.getDepth) _
    context.setInst(inst)
    val result = debugWithMessage(message)(stackFrame)(block)
    context.resetInst()
    result
  }

  protected def innerDebugMatches(matches: Boolean): Unit = {

    def mkMessage(depth: Int): String = {
      val tabs = "\t" * depth
      val extractorString = "[]"
//      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
//          .replace("#", s"$extractorString.")
//          .replace(' ', '.')
//      val instString = inst.toString
//      val message = s"""${tabs}${side} $where(inst = $instString)"""
      val message = s"""${tabs}$matches"""

      message
    }

    val message = mkMessage(context.getDepth)

    if (active) {
      if (!quiet) println(message)
      transcript += context.setMatches(matches)
    }
  }

  def showTrace(stack: Debugger.Stack): Unit = {
    println("Here's your trace...")
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

  def debugMatches(matches: Boolean): Unit = innerDebugMatches(matches)
}

object Debugger {
  type Stack = List[StackFrame]
}