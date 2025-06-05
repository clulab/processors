package org.clulab.odin.debugger.odin

import org.clulab.odin.State
import org.clulab.odin.debugger.Debugger
import org.clulab.odin.impl.{Inst, TokenPattern}
import org.clulab.odin.impl.TokenPattern.{GlobalCapture, Result}
import org.clulab.processors.Document

class DebuggingTokenPattern(debugger: Debugger, start: Inst, source: String) extends TokenPattern(start, Some(source)) {

  override def findPrefixOf(tok: Int, sent: Int, doc: Document, state: State): Seq[Result] = {
    DebuggingThompsonVM.evaluate(debugger, start, tok, sent, doc, state) map {
      case (groups, mentions) =>
        // There must be one GlobalCapture only.
        val globalCapture = groups(GlobalCapture).head
        Result(globalCapture, groups - GlobalCapture, mentions)
    }
  }

  override def findFirstIn(tok: Int, sent: Int, doc: Document, state: State): Seq[Result] = {
    val n = doc.sentences(sent).size

    @annotation.tailrec
    def loop(i: Int): Seq[Result] = {
      if (i < n) {
        val r = debugger.debugStart(i) {
          findPrefixOf(i, sent, doc, state)
        }

        if (r.nonEmpty) r
        else loop(i + 1)
      }
      else Nil
    }

    loop(tok)
  }
}

object DebuggingTokenPattern {

  def apply(debugger: Debugger, tokenPattern: TokenPattern): DebuggingTokenPattern = {
    new DebuggingTokenPattern(
      debugger,
      tokenPattern.start,
      tokenPattern.sourceOpt.get
    )
  }
}
