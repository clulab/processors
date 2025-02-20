package org.clulab.odin.debugger.debug

case class ThreadMatch(matches: Boolean, reason: String)

object ThreadMatch {
  val instMismatch = ThreadMatch(false, "Inst mismatch")
  val empty = ThreadMatch(false, "Thread empty")
  val lowerPriority = ThreadMatch(false, "Thread not the best")
  val superseded = ThreadMatch(false, "Thread superseded")
  val survivor = ThreadMatch(true, "Thread survived")
}
