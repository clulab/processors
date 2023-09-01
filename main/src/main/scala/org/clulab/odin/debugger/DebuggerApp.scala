package org.clulab.odin.debugger

import scala.util.matching.Regex

// This is an experimental playground.
object DebuggerApp extends App {

  def lowest(regex: Regex): Unit = Debugger.debug {
    // I don't do anything.
  }

  def lower(depth: Int): Float = Debugger.debug(SpecialStackFrame("This is special")) { stackFrame =>
    stackFrame.showMessage()
    Debugger.showTrace()
    lowest("hello".r)
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
  Debugger.showDeepest()
}
