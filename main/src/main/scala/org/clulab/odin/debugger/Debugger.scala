package org.clulab.odin.debugger

import org.clulab.odin.ExtractorEngine
import org.clulab.odin.impl.{Extractor, Inst}
import org.clulab.processors.{Document, Sentence}
import org.clulab.utils.{StringUtils, Timer}

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

// TODO: This needs to be made tread-safe!
// Each Odin instance could have its own, for example.
class DebuggerContext(
  protected var documentOpt: Option[Document] = None,
  protected var loopOpt: Option[Int] = None,
  protected var extractorOpt: Option[Extractor] = None,
  protected var sentenceIndexOpt: Option[Int] = None,
  protected var sentenceOpt: Option[Sentence] = None,
  protected var startOpt: Option[Int] = None,
  protected var tokOpt: Option[Int] = None,
  protected var instOpt: Option[Inst] = None
) {
  def isComplete: Boolean = {
    documentOpt.isDefined &&
    loopOpt.isDefined &&
    extractorOpt.isDefined &&
    sentenceIndexOpt.isDefined &&
    sentenceOpt.isDefined &&
    startOpt.isDefined &&
    tokOpt.isDefined &&
    instOpt.isDefined
  }

  def setDocument(document: Document): Unit = {
    assert(documentOpt.isEmpty)
    documentOpt = Some(document)
  }

  def getDocumentOpt: Option[Document] = documentOpt

  def resetDocument(): Unit = documentOpt = None

  def setLoop(loop: Int): Unit = {
    assert(documentOpt.nonEmpty)
    assert(loopOpt.isEmpty)
    loopOpt = Some(loop)
  }

  def getLoopOpt: Option[Int] = loopOpt

  def resetLoop(): Unit = loopOpt = None

  def setExtractor(extractor: Extractor): Unit = {
    assert(loopOpt.nonEmpty)
    assert(extractorOpt.isEmpty)
    extractorOpt = Some(extractor)
  }

  def getExtractorOpt: Option[Extractor] = extractorOpt

  def resetExtractor(): Unit = extractorOpt = None

  def setSentence(sentenceIndex: Int, sentence: Sentence): Unit = {
    assert(loopOpt.nonEmpty)
    assert(sentenceIndexOpt.isEmpty)
    assert(sentenceOpt.isEmpty)
    sentenceIndexOpt = Some(sentenceIndex)
    sentenceOpt = Some(sentence)
  }

  def getSentenceOpt: (Option[Int], Option[Sentence]) = (sentenceIndexOpt, sentenceOpt)

  def resetSentence(): Unit = {
    sentenceIndexOpt = None
    sentenceOpt = None
  }

  def setStart(start: Int): Unit = {
    assert(sentenceIndexOpt.nonEmpty)
    assert(sentenceOpt.nonEmpty)
    startOpt = Some(start)
  }

  def getStartOpt: Option[Int] = startOpt

  def resetStart(): Unit = startOpt = None

  def setTok(tok: Int): Unit = {
    assert(startOpt.nonEmpty)
    assert(tokOpt.isEmpty)
    tokOpt = Some(tok)
  }

  def getTokOpt: (Option[Int]) = tokOpt

  def resetTok(): Unit = {
    tokOpt = None
  }

  def setInst(inst: Inst): Unit = {
    assert(tokOpt.nonEmpty)
    assert(instOpt.isEmpty)
    instOpt = Some(inst)
  }

  def getInstOpt: Option[Inst] = instOpt

  def resetInst(): Unit = instOpt = None

  def setMatches(matches: Boolean): DebuggerRecord = {
    assert(isComplete)
    DebuggerRecord(documentOpt.get, loopOpt.get, extractorOpt.get, sentenceIndexOpt.get, sentenceOpt.get,
      startOpt.get, tokOpt.get, instOpt.get, matches)
  }
}

trait DebuggerTrait {
  def activate(): Unit
  def deactivate(): Unit
  def showTrace(): Unit
  def showDeepest(): Unit
  // TODO break or pause
}

class Debugger protected () extends DebuggerTrait {
  protected var active: Boolean = true // TODO: You can turn off debugging with this!
  protected var stack: Debugger.Stack = List()
  protected var maxDepth = 0
  protected var maxStack: Debugger.Stack = stack
  protected val context = new DebuggerContext()
  protected val transcript: Buffer[DebuggerRecord] = Buffer.empty

  def activate(): Unit = active = true

  def deactivate(): Unit = active = false

  def debug[ResultType, StackFrameType <: StackFrame](stackFrame: StackFrameType)(block: => ResultType): ResultType = {
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

  def debugWithMessage[ResultType, StackFrameType <: StackFrame](mkMessage: (String) => String)
      (stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    if (active) {
      println(mkMessage("beg"))
      val result = debug(stackFrame)(block)
      println(mkMessage("end"))
      result
    }
    else {
      block
    }
  }

  def debugDoc[ResultType, StackFrameType <: StackFrame](extractorEngine: ExtractorEngine, doc: Document)
      (stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(side: String): String = {
      val tabs = "\t" * 0
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
          .replace("#", s"$extractorString.")
      val docString = StringUtils.afterLast(doc.toString, '.')
      val textString = doc.text.getOrElse(doc.sentences.map { sentence => sentence.words.mkString(" ") }.mkString(" "))
      val message = s"""${tabs}${side} $where(doc = $docString("$textString"))"""

      message
    }

    context.setDocument(doc)
    val result = debugWithMessage(mkMessage)(stackFrame)(block)
    context.resetDocument()

    result
  }

  def debugLoop[ResultType, StackFrameType <: StackFrame](loop: Int)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(side: String): String = {
      val tabs = "\t" * 1
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
          .replace("#", s"$extractorString.")
          .replace(' ', '.')
      val loopString = loop.toString
      val message = s"""${tabs}${side} $where(loop = $loopString)"""

      message
    }

    context.setLoop(loop)
    val result = debugWithMessage(mkMessage)(stackFrame)(block)
    context.resetLoop()
    result
  }

  def debugExtractor[ResultType, StackFrameType <: StackFrame](extractor: Extractor)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    // TODO: This could keep track of the mentions returned from the block and
    // do something special if there are none.

    // TODO: This could be part of a stack frame, no longer generic, like the TokenExtractorFrame.
    def mkMessage(side: String): String = {
      val tabs = "\t" * 2
      val extractorString = s"""["${extractor.name}"]"""
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
        .replace("#", s"$extractorString.")
      val message = s"""${tabs}${side} $where()"""

      message
    }

    context.setExtractor(extractor)
    val result = debugWithMessage(mkMessage)(stackFrame)(block)
    context.resetExtractor()
    result
  }

  def debugSentence[ResultType, StackFrameType <: StackFrame](index: Int, sentence: Sentence)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(side: String): String = {
      val tabs = "\t" * 3
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
        .replace("#", s"$extractorString.")
        .replace(' ', '.')
      val sentenceString = sentence.words.mkString(" ")
      val message = s"""${tabs}${side} $where(index = $index, sentence = "$sentenceString")"""

      message
    }

    context.setSentence(index, sentence)
    val result = debugWithMessage(mkMessage)(stackFrame)(block)
    context.resetSentence()
    result
  }

  def debugStart[ResultType, StackFrameType <: StackFrame](start: Int)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(side: String): String = {
      val tabs = "\t" * 4
      val method = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
      val obj = StringUtils.afterLast(StringUtils.beforeLast(stackFrame.sourceCode.enclosing.value, '.'), '.')
      val message = s"""${tabs}${side} $obj.$method(start = $start)"""

      message
    }

    context.setStart(start)
    val result = debugWithMessage(mkMessage)(stackFrame)(block)
    context.resetStart()
    result
  }

  def debugTok[ResultType, StackFrameType <: StackFrame](tok: Int)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(side: String): String = {
      val tabs = "\t" * 5
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
        .replace("#", s"$extractorString.")
        .replace(' ', '.')
      val message = s"""${tabs}${side} $where(tok = $tok)"""

      message
    }

    context.setTok(tok)
    val result = debugWithMessage(mkMessage)(stackFrame)(block)
    context.resetTok()
    result
  }

  def debugInst[ResultType, StackFrameType <: StackFrame](inst: Inst)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(side: String): String = {
      val tabs = "\t" * 6
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
          .replace("#", s"$extractorString.")
          .replace(' ', '.')
      val instString = inst.toString
      val message = s"""${tabs}${side} $where(inst = $instString)"""

      message
    }

    context.setInst(inst)
    val result = debugWithMessage(mkMessage)(stackFrame)(block)
    context.resetInst()
    result
  }

  def debugMatches(matches: Boolean): Unit = {
    transcript += context.setMatches(matches)
  }

  def showTrace(stack: Debugger.Stack): Unit = {
    println("Here's your trace...")
    stack.zipWithIndex.foreach { case (stackFrame, index) =>
      println(s"$index: $stackFrame")
    }
  }

  def showTrace(): Unit = showTrace(stack)

  def showDeepest(): Unit = showTrace(maxStack)
}

object Debugger extends DebuggerTrait {
  type Stack = List[StackFrame]

  lazy val instance = new Debugger() // TODO: In the end this will not be global unless it can be thread-aware.

  override def activate(): Unit = instance.activate()

  override def deactivate(): Unit = instance.deactivate()

  def debugFrame[ResultType, StackFrameType <: StackFrame](stackFrame: StackFrameType)(block: StackFrameType => ResultType): ResultType = {
    instance.debug(stackFrame)(block(stackFrame))
  }

  def debug[ResultType, StackFrameType <: StackFrame](stackFrame: StackFrameType)(block: => ResultType): ResultType = {
    instance.debug(stackFrame)(block)
  }

  def debug[ResultType](block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debug(stackFrame)(block)
  }

  // The extractorEngine needs to come in with the doc so that client code does not need to call debug
  // methods on the extractorEngine itself.
  def debugDoc[ResultType](extractorEngine: ExtractorEngine, doc: Document)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugDoc(extractorEngine, doc)(stackFrame)(block)
  }

  def debugLoop[ResultType](loop: Int)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugLoop(loop)(stackFrame)(block)
  }

  def debugExtractor[ResultType](extractor: Extractor)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugExtractor(extractor)(stackFrame)(block)
  }

  // TODO: We should know the document already, so the index should suffice.
  def debugSentence[ResultType](index: Int, sentence: Sentence)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugSentence(index, sentence)(stackFrame)(block)
  }

  def debugStart[ResultType](start: Int)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugStart(start)(stackFrame)(block)
  }

  def debugTok[ResultType](tok: Int)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugTok(tok)(stackFrame)(block)
  }

  def debugInst[ResultType](inst: Inst)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugInst(inst)(stackFrame)(block)
  }

  def debugMatches(matches: Boolean) = instance.debugMatches(matches)

  def showTrace(): Unit = {
    instance.showTrace()
  }

  def showDeepest(): Unit = {
    instance.showDeepest()
  }
}
