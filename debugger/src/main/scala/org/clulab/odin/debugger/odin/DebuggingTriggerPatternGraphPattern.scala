package org.clulab.odin.debugger.odin

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.impl.{ArgumentPattern, OdinConfig, TriggerPatternGraphPattern}
import org.clulab.odin.{EventMention, Mention, State, TextBoundMention, mkTokenInterval}
import org.clulab.processors.Document
import org.clulab.struct.Interval

class DebuggingTriggerPatternGraphPattern(
  val debugger: Debugger,
  debuggingTrigger: DebuggingTokenPattern,
  arguments: Seq[ArgumentPattern], // TODO
  config: OdinConfig
) extends TriggerPatternGraphPattern(debuggingTrigger, arguments, config) {

  override def getMentions(
    sent: Int,
    doc: Document,
    state: State,
    labels: Seq[String],
    keep: Boolean,
    ruleName: String
  ): Seq[EventMention] = {
    debugger.debugTrigger(trigger) {
      debugger.debugTokenPattern(trigger) {
        val tokenPatternResults = trigger.findAllIn(sent, doc, state)
        val eventMentions = tokenPatternResults.flatMap { tokenPatternResult =>
          val tokenInterval = Interval(tokenPatternResult.start, tokenPatternResult.end)
          val eventMentions = debugger.debugTokenInterval(tokenInterval) {
            lazy val tbmTrigger = new TextBoundMention(labels, tokenInterval, sent, doc, keep, ruleName)
            // Is this a good place to record mention comparisons?
            // This might necessitate an Arguments View.
            val arguments = extractArguments(tokenInterval, sent, doc, state)
            val eventMentions = arguments.map { case (args, paths) =>
              new EventMention(labels, mkTokenInterval(tbmTrigger, args), tbmTrigger, args, paths, sent, doc, keep, ruleName)
            }

            eventMentions
          }

          eventMentions
        }

        eventMentions
      }
    }
  }
}

object DebuggingTriggerPatternGraphPattern {

  def apply(debugger: Debugger, triggerPatternGraphPattern: TriggerPatternGraphPattern): DebuggingTriggerPatternGraphPattern = {
    new DebuggingTriggerPatternGraphPattern(
      debugger,
      DebuggingTokenPattern(debugger, triggerPatternGraphPattern.trigger),
      triggerPatternGraphPattern.arguments,
      triggerPatternGraphPattern.config
    )
  }
}
