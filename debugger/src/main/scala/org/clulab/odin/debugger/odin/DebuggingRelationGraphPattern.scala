package org.clulab.odin.debugger.odin

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.debugger.debug.matches.MentionMatch
import org.clulab.odin.impl.{ArgumentPattern, GraphPattern, OdinConfig, RelationGraphPattern}
import org.clulab.odin.{Mention, RelationMention, State, mkTokenInterval}
import org.clulab.processors.Document
import org.clulab.struct.Interval

class DebuggingArgumentExtractor(debugger: Debugger) extends GraphPattern {
  override def arguments: Seq[ArgumentPattern] = ???
  override val config: OdinConfig = ???
  override def getMentions(sent: Int, doc: Document, state: State, labels: Seq[String], keep: Boolean, ruleName: String): Seq[Mention] = ???

  override def extractArguments(
    tokens: Interval,
    sent: Int,
    doc: Document,
    state: State
  ): Seq[(Args, Paths)] = {
    super.extractArguments(tokens, sent, doc, state)
  }
}

class DebuggingRelationGraphPattern(
  val debugger: Debugger,
  anchorName: String,
  anchorLabel: String,
  arguments: Seq[ArgumentPattern],  // TODO
  config: OdinConfig
) extends RelationGraphPattern(anchorName, anchorLabel, arguments, config) {
  //val debuggingArgumentExtractor = new DebuggingArgumentExtractor(debugger)
  val debuggingMention = DebuggingMention(anchorName, arguments)

  def getMatchingMentionsFromState(state: State, sent: Int, allStateMentions: Seq[Mention]): Seq[Mention] = {
    val mentionMentions = state.mentionsFor(sent)
    val labelMentions = mentionMentions.filter { stateMention =>
      stateMention.matches(anchorLabel)
    }

    val mentionMatches = allStateMentions.map { stateMention =>
      if (!mentionMentions.contains(stateMention))
        MentionMatch.stateMismatch
      else if (!labelMentions.contains(stateMention))
        MentionMatch.labelMismatch
      else
        MentionMatch.mentionMatch
    }

    debugger.debugMentionMatches(debuggingMention, allStateMentions, mentionMatches)
    labelMentions
  }

  override def getMentions(
    sent: Int,
    doc: Document,
    state: State,
    labels: Seq[String],
    keep: Boolean,
    ruleName: String
  ): Seq[RelationMention] = {
    val allStateMentions = state.allMentions // Only get this once.
    val matchingMentions = getMatchingMentionsFromState(state, sent, allStateMentions)
    val relationMentions = matchingMentions.flatMap { mention =>
      val arguments = /*debuggingArgumentExtractor.*/extractArguments(mention.tokenInterval, sent, doc, state)
      val relationMentions = arguments.map { case (args, paths) =>
        val relationArgs = args + (anchorName -> Seq(mention))
        val relationPaths = paths + (anchorName -> Map(mention -> Nil))

        new RelationMention(labels, mkTokenInterval(relationArgs), relationArgs, relationPaths, sent, doc, keep, ruleName)
      }

      relationMentions
    }

    relationMentions
  }
}

object DebuggingRelationGraphPattern {

  def apply(debugger: Debugger, relationGraphPattern: RelationGraphPattern): DebuggingRelationGraphPattern = {
    new DebuggingRelationGraphPattern(
      debugger,
      relationGraphPattern.anchorName,
      relationGraphPattern.anchorLabel,
      relationGraphPattern.arguments,
      relationGraphPattern.config
    )
  }
}
