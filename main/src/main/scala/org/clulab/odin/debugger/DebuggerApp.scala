package org.clulab.odin.debugger

object DebuggerApp extends App {

  class SpecialStackFrame(message: String, sourceCode: SourceCode) extends StackFrame(sourceCode) {

    def showMessage(): Unit = println(message)
  }

  object SpecialStackFrame {

    def apply(message: String)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): SpecialStackFrame = {
      new SpecialStackFrame(message, new SourceCode(line, fileName, enclosing))
    }
  }

  def lower(depth: Int): Float = Debugger.debug(SpecialStackFrame("This is special")) { stackFrame =>
    stackFrame.showMessage()
    Debugger.trace
    depth.toFloat
  }

  def subroutine(message: String): String = Debugger.debug {
    println(s"Odin says $message")
    lower(6)
    message
  }

  def odin(): String = Debugger.debug {
    subroutine("Keith says hello")
  }

  odin()
}
