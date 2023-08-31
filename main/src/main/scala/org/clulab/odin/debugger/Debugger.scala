package org.clulab.odin.debugger

import org.clulab.utils.Lazy

class Debugger protected () {
  val active = true // TODO: You can turn off debugging with this!
  var stack = List[StackFrame]()

  def debug[StackFrameT <: StackFrame, T](stackFrame: StackFrameT, lazyBlock: Lazy[T]): T = {
    stack = stackFrame :: stack

    val result = try {
      lazyBlock.value
    }
    finally {
      stack = stack.tail
    }
    result
  }

  def trace(): Unit = {
    stack.zipWithIndex.foreach { case (stackFrame, index) =>
      println(s"$index: $stackFrame")
    }
  }
}

object Debugger {
  lazy val instance = new Debugger() // TODO: In the end this will not be global unless it can be thread-aware.

  def debug[StackFrameType <: StackFrame, ResultType](stackFrame: StackFrameType)(block: StackFrameType => ResultType): ResultType = {
    instance.debug(stackFrame, Lazy(block(stackFrame)))
  }

  def debug[ResultType](block: => ResultType)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): ResultType = {
    val sourceCode = new SourceCode(line, fileName, enclosing)
    val stackFrame = new StackFrame(sourceCode)

    instance.debug(stackFrame, Lazy(block))
  }

  def trace(): Unit = {
    instance.trace()
  }
}
