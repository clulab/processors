package org.clulab.odin.debugger.odin

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.debugger.debug.matches.MentionMatch
import org.clulab.odin.impl.{ArgumentPattern, OdinConfig, TriggerMentionGraphPattern}
import org.clulab.odin.{EventMention, Mention, State, TextBoundMention, mkTokenInterval}
import org.clulab.processors.Document

class DebuggingTriggerMentionGraphPattern(
  val debugger: Debugger,
  triggerLabel: String,
  arguments: Seq[ArgumentPattern],
  config: OdinConfig
) extends TriggerMentionGraphPattern(triggerLabel, arguments, config) {
  val debuggingMention = DebuggingMention(triggerLabel, arguments)

  def getMatchingMentionsFromState(state: State, sent: Int, allStateMentions: Seq[Mention]): Seq[TextBoundMention] = {
    val mentionMentions = state.mentionsFor(sent)
    val labelMentions = mentionMentions.filter { stateMention =>
      stateMention.matches(triggerLabel)
    }
    val instancesOfTextBoundMentions = labelMentions.filter { stateMention =>
      stateMention.isInstanceOf[TextBoundMention]
    }
    val textBoundMentions = instancesOfTextBoundMentions.map(_.asInstanceOf[TextBoundMention])

    val mentionMatches = allStateMentions.map { stateMention =>
      if (!mentionMentions.contains(stateMention))
        MentionMatch.stateMismatch
      else if (!labelMentions.contains(stateMention))
        MentionMatch.labelMismatch
      else if (!instancesOfTextBoundMentions.contains(stateMention))
        MentionMatch.typeMismatch
      else
        MentionMatch.mentionMatch
    }

    debugger.debugMentionMatches(debuggingMention, allStateMentions, mentionMatches)
    textBoundMentions
  }

  override def getMentions(
    sent: Int,
    doc: Document,
    state: State,
    labels: Seq[String],
    keep: Boolean,
    ruleName: String
  ): Seq[EventMention] = {
    val allStateMentions = state.allMentions // Only get this once.
    val matchingMentions = getMatchingMentionsFromState(state, sent, allStateMentions)
    val eventMentions = matchingMentions.flatMap { textBoundMention =>
      val trig = textBoundMention
      val arguments = extractArguments(trig.tokenInterval, sent, doc, state)
      val eventMentions = arguments.map { case (args, paths) =>
        new EventMention(labels, mkTokenInterval(trig, args), trig, args, paths, sent, doc, keep, ruleName)
      }

      eventMentions
    }

    eventMentions
  }
}

object DebuggingTriggerMentionGraphPattern {

  def apply(debugger: Debugger, triggerMentionGraphPattern: TriggerMentionGraphPattern): DebuggingTriggerMentionGraphPattern = {
    new DebuggingTriggerMentionGraphPattern(
      debugger,
      triggerMentionGraphPattern.triggerLabel,
      triggerMentionGraphPattern.arguments,
      triggerMentionGraphPattern.config
    )
  }
}
