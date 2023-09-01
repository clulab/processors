package org.clulab.odin.debugger

class StackFrame(sourceCode: SourceCode) {

  override def toString: String = s"${getClass.getName}\t$sourceCode"
}

class SpecialStackFrame(message: String, sourceCode: SourceCode) extends StackFrame(sourceCode) {

  def showMessage(): Unit = println(message)
}

object SpecialStackFrame {

  def apply(message: String)(implicit line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): SpecialStackFrame = {
    new SpecialStackFrame(message, new SourceCode(line, fileName, enclosing))
  }
}
