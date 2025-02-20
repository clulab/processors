package org.clulab.odin.debugger.debug

// See https://github.com/com-lihaoyi/sourcecode

class SourceCode(val line: sourcecode.Line, val fileName: sourcecode.FileName, val enclosing: sourcecode.Enclosing) {

  override def toString: String = {
    s"${enclosing.value}(${fileName.value}:${line.value})"
  }
}

object SourceCode {

  def apply(line: sourcecode.Line, fileName: sourcecode.FileName, enclosing: sourcecode.Enclosing): SourceCode = {
    new SourceCode(line, fileName, enclosing)
  }
}
