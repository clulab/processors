package org.clulab.odin.debugger.debugging

import org.clulab.odin.{Action, Mention, State}
import org.clulab.odin.debugger.Debugger
import org.clulab.odin.impl.{CrossSentenceExtractor, OdinException, Priority, TokenExtractor}
import org.clulab.processors.Document

class DebuggingCrossSentenceExtractor(
  val debugger: Debugger,
  val crossSentenceExtractor: CrossSentenceExtractor,
  name: String,
  labels: Seq[String],
  priority: Priority,
  keep: Boolean,
  action: Action,
  leftWindow: Int,
  rightWindow: Int,
  anchorPattern: TokenExtractor,
  neighborPattern: TokenExtractor,
  anchorRole: String,
  neighborRole: String
) extends CrossSentenceExtractor(name, labels, priority, keep, action, leftWindow, rightWindow, anchorPattern,
    neighborPattern, anchorRole, neighborRole) {
  // check for valid window values
  if (leftWindow < 0) throw OdinException(s"left-window for '$name' must be >= 0")
  if (rightWindow < 0) throw OdinException(s"right-window for '$name' must be >= 0")

  // This comes indirectly through Extractor.
  override def findAllIn(doc: Document, state: State): Seq[Mention] = debugger.debugExtractor(crossSentenceExtractor) {
    super.findAllIn(doc, state)
  }

  def getMatchingMentionsFromState(state: State, mention: Mention): Seq[Mention] = {
    state.mentionsFor(mention.sentence, mention.tokenInterval).filter { stateMention =>
      // the span should match exactly
      (stateMention.tokenInterval == mention.tokenInterval) &&
        // the label should match
        (stateMention matches mention.label)
    }
  }

  override def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = debugger.debugSentence(sent, doc.sentences(sent)) {
    val labelAnchorMentions = debugger.debugAnchor(anchorPattern) {
      debugger.debugExtractor(anchorPattern) {
        val allAnchorMentions = anchorPattern.findAllIn(sent, doc, state)

        // Find the mentions in the state that match the given span and label.
        allAnchorMentions.flatMap { mention => getMatchingMentionsFromState(state, mention) }
      }
    }

    if (labelAnchorMentions.isEmpty) Nil // the rule failed
    else debugger.debugNeighbor(neighborPattern) {
      val windowRange = Range(math.max(sent - leftWindow, 0), math.min(sent + rightWindow, doc.sentences.length))
      val labelNeighborMentions = windowRange.flatMap { i =>
        if (i == sent) Seq.empty // The neighbor cannot be in the same sentence as the anchor.
        else {
          debugger.debugSentence(i, doc.sentences(i)) {
            debugger.debugExtractor(neighborPattern) {
              val allNeighborMentions = neighborPattern.findAllIn(i, doc, state)

              // Find the mentions in the state that match the given span and label.
              allNeighborMentions.flatMap { mention => getMatchingMentionsFromState(state, mention) }
            }
          }
        }
      }
      val crossSentenceMentions = labelAnchorMentions.flatMap { anchor =>
        labelNeighborMentions.map { neighbor =>
          mkMention(anchor, neighbor)
        }
      }

      action(crossSentenceMentions, state)
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
      crossSentenceExtractor.action,
      crossSentenceExtractor.leftWindow,
      crossSentenceExtractor.rightWindow,
      DebuggingTokenExtractor(debugger, crossSentenceExtractor.anchorPattern),
      DebuggingTokenExtractor(debugger, crossSentenceExtractor.neighborPattern),
      crossSentenceExtractor.anchorRole,
      crossSentenceExtractor.neighborRole
    )
  }
}
