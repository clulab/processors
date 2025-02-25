package org.clulab.odin.debugger.odin

import org.clulab.odin.{Mention, State}
import org.clulab.odin.debugger.Debugger
import org.clulab.odin.debugger.debug.MentionMatch
import org.clulab.odin.impl.{CrossSentenceExtractor, OdinException, Priority}
import org.clulab.processors.Document

class DebuggingCrossSentenceExtractor(
  override val debugger: Debugger,
  val crossSentenceExtractor: CrossSentenceExtractor,
  name: String,
  labels: Seq[String],
  priority: Priority,
  keep: Boolean,
  debuggingAction: DebuggingAction,
  leftWindow: Int,
  rightWindow: Int,
  debuggingAnchorExtractor: DebuggingTokenExtractor,
  debuggingNeighborExtractor: DebuggingTokenExtractor,
  anchorRole: String,
  neighborRole: String,
  ruleOpt: Option[String]
) extends CrossSentenceExtractor(name, labels, priority, keep, debuggingAction, leftWindow, rightWindow, debuggingAnchorExtractor,
    debuggingNeighborExtractor, anchorRole, neighborRole, ruleOpt) with DebuggingExtractor {
  // Check for valid window values.
  if (leftWindow < 0) throw OdinException(s"left-window for '$name' must be >= 0")
  if (rightWindow < 0) throw OdinException(s"right-window for '$name' must be >= 0")

  def extractor: CrossSentenceExtractor = crossSentenceExtractor

  override def withDebuggingChildren: Seq[DebuggingExtractor] = Seq(this, debuggingAnchorExtractor, debuggingNeighborExtractor)

  // This comes indirectly through Extractor.
  override def findAllIn(doc: Document, state: State): Seq[Mention] = /*debugger.debugExtractor(crossSentenceExtractor)*/ {
    super.findAllIn(doc, state)
  }

  def getMatchingMentionsFromState(state: State, mention: Mention, allStateMentions: Seq[Mention]): Seq[Mention] = {
    val mentionMentions = state.mentionsFor(mention.sentence, mention.tokenInterval)
    val intervalMentions = mentionMentions.filter { stateMention =>
      stateMention.tokenInterval == mention.tokenInterval
    }
    val labelMentions = intervalMentions.filter { stateMention =>
      stateMention.matches(mention.label)
    }

    val mentionMatches = allStateMentions.map { stateMention =>
      if (!mentionMentions.contains(stateMention))
        MentionMatch.stateMismatch
      else if (!intervalMentions.contains(stateMention))
        MentionMatch.intervalMismatch
      else if (!labelMentions.contains(stateMention))
        MentionMatch.labelMismatch
      else
        MentionMatch.mentionMatch
    }

    debugger.debugMentionMatches(mention, allStateMentions, mentionMatches)
    labelMentions
  }

  def findAllInExtractor(debuggingTokenExtractor: DebuggingTokenExtractor, windowRange: Seq[Int], doc: Document, state: State): Seq[Mention] = {
    val mentions = debugger.debugExtractor(debuggingTokenExtractor.extractor) {
      debugger.debugTokenPattern(debuggingTokenExtractor.pattern) {
        windowRange.flatMap { sent =>
          debuggingTokenExtractor.findAllIn(sent, doc, state)
        }
      }
    }

    mentions
  }

  def innerFindAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = {
    val windowRange = Range(
      math.max(sent - leftWindow, 0),
      math.min(sent + rightWindow, doc.sentences.length)
    ).filterNot(_ == sent) // The neighbor cannot be in the same sentence as the anchor.
    val allStateMentions = state.allMentions // Only get this once.

    val allAnchorMentions = findAllInExtractor(debuggingAnchorExtractor, Seq(sent), doc, state)
    // When debugging, we look for the neighbors even if there are no anchors to pair them with.
    val allNeighborMentions = findAllInExtractor(debuggingNeighborExtractor, windowRange, doc, state)

    // Find the mentions in the state that match the given span and label.
    val someAnchorMentions = allAnchorMentions.flatMap { mention => getMatchingMentionsFromState(state, mention, allStateMentions) }
    val someNeighborMentions = allNeighborMentions.flatMap { mention => getMatchingMentionsFromState(state, mention, allStateMentions) }

    val crossSentenceMentions = someAnchorMentions.flatMap { anchor =>
      someNeighborMentions.map { neighbor =>
        mkMention(anchor, neighbor)
      }
    }
    val actionedCrossSentenceMentions = debuggingAction(crossSentenceMentions, state)

    actionedCrossSentenceMentions
  }

  override def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = {
    debugger.debugSentence(sent, doc.sentences(sent)) {
      debugger.debugExtractor(extractor) {
        innerFindAllIn(sent, doc, state)
      }
    }
  }
}

object DebuggingCrossSentenceExtractor {

  def apply(debugger: Debugger, crossSentenceExtractor: CrossSentenceExtractor): DebuggingCrossSentenceExtractor = {
    new DebuggingCrossSentenceExtractor(
      debugger,
      crossSentenceExtractor,
      crossSentenceExtractor.name,
      crossSentenceExtractor.labels,
      crossSentenceExtractor.priority,
      crossSentenceExtractor.keep,
      DebuggingAction(debugger, crossSentenceExtractor.action, Some(crossSentenceExtractor)),
      crossSentenceExtractor.leftWindow,
      crossSentenceExtractor.rightWindow,
      DebuggingTokenExtractor(debugger, crossSentenceExtractor.anchorPattern),
      DebuggingTokenExtractor(debugger, crossSentenceExtractor.neighborPattern),
      crossSentenceExtractor.anchorRole,
      crossSentenceExtractor.neighborRole,
      crossSentenceExtractor.ruleOpt
    )
  }
}
