package org.clulab.odin.debugger.odin

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.impl.Extractor
import org.clulab.odin.{Action, Mention, State}

class DebuggingAction(debugger: Debugger, action: Action, val extractorOpt: Option[Extractor]) extends Action {

  def apply(mentions: Seq[Mention], state: State): Seq[Mention] = {
    val inMentions = mentions
    val outMentions = action(mentions, state)

    // TODO: Report the extractor in use or None
    debugger.debugAction(inMentions, outMentions)
    outMentions
  }
}

object DebuggingAction {

  def apply(debugger: Debugger, action: Action, extractorOpt: Option[Extractor]): DebuggingAction = new DebuggingAction(debugger, action, extractorOpt)
}
