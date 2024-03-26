package org.clulab.odin.debugger
import org.clulab.odin.ExtractorEngine
import org.clulab.odin.impl.{CrossSentenceExtractor, Done, Extractor, GraphExtractor, Inst, MatchLookAhead, MatchLookBehind, MatchMention, MatchSentenceEnd, MatchSentenceStart, MatchToken, Pass, SaveEnd, SaveStart, Split, TokenExtractor}
import org.clulab.processors.{Document, Sentence}
import org.clulab.utils.{StringUtils, Timer}

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

    debugWithMessage(mkMessage)(stackFrame)(block)
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

    debugWithMessage(mkMessage)(stackFrame)(block)
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

    debugWithMessage(mkMessage)(stackFrame)(block)
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

    debugWithMessage(mkMessage)(stackFrame)(block)
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

    debugWithMessage(mkMessage)(stackFrame)(block)
  }

  def debugTokInst[ResultType, StackFrameType <: StackFrame](tok: Int, inst: Inst)(stackFrame: StackFrameType)(block: => ResultType): ResultType = {

    // TODO: This could be part of a stack frame, no longer generic.
    def mkMessage(side: String): String = {
      val tabs = "\t" * 5
      val extractorString = "[]"
      val where = StringUtils.afterLast(stackFrame.sourceCode.enclosing.value, '.')
        .replace("#", s"$extractorString.")
        .replace(' ', '.')
      val instString = inst.toString
      val message = s"""${tabs}${side} $where(tok = $tok, inst = $instString)"""

      message
    }

    debugWithMessage(mkMessage)(stackFrame)(block)
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

  def debugTokInst[ResultType](tok: Int, inst: Inst)(block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debugTokInst(tok, inst)(stackFrame)(block)
  }

  def showTrace(): Unit = {
    instance.showTrace()
  }

  def showDeepest(): Unit = {
    instance.showDeepest()
  }

  def visualize(extractor: Extractor, sentence: String): Unit = {
    extractor match {
      case tokenExtractor: TokenExtractor =>
        println(s"\nThere was an extractor: ${tokenExtractor.name}")
        visualizeExtractor(tokenExtractor.pattern.start, "", sentence, 0)
      case graphExtractor: GraphExtractor => println("\nThere was a graph extractor.")
      case crossSentenceExtractor: CrossSentenceExtractor =>
        visualizeExtractor(crossSentenceExtractor.anchorPattern.pattern.start, s"${crossSentenceExtractor.name} (Anchor)", sentence, 0)
        visualizeExtractor(crossSentenceExtractor.neighborPattern.pattern.start, s"${crossSentenceExtractor.name} (Neighbor)", sentence, 0)
      case _ => println("Unknown extractor type")
    }
  }

  private def visualizeExtractor(inst: Inst, name: String, sentence: String, depth: Int): Unit = {
    val indent = " " * (depth * 3) + name

    def loopsOrDeadEnds(nextInst: Inst): Boolean = {
      nextInst == null || (nextInst.getPosId <= inst.getPosId && nextInst.getPosId != 0)
    }

    val visualization = indent + inst.visualize(sentence)
    println(visualization)

    inst match {
      case split: Split =>
        if (!loopsOrDeadEnds(split.lhs))
          visualizeExtractor(split.lhs, "(LHS) ", sentence, depth + 1)
        if (!loopsOrDeadEnds(split.rhs))
          visualizeExtractor(split.rhs, "(RHS) ", sentence, depth + 1)

      case saveStart: SaveStart =>

      case saveEnd: SaveEnd =>

      case matchToken: MatchToken =>

      case matchMention: MatchMention =>

      case sentenceStart: MatchSentenceStart =>

      case sentenceEnd: MatchSentenceEnd =>

      case pass: Pass =>

      case Done =>

      case lookAhead: MatchLookAhead =>
        if (!loopsOrDeadEnds(lookAhead.start))
          visualizeExtractor(lookAhead.start, "(Start) ", sentence, depth + 1)

      case lookBehind: MatchLookBehind =>
        if (!loopsOrDeadEnds(lookBehind.start))
          visualizeExtractor(lookBehind.start, "(Start) ", sentence, depth + 1)

      case _ =>
    }
    if (!loopsOrDeadEnds(inst.getNext))
      visualizeExtractor(inst.getNext, name, sentence, depth)
  }
}
