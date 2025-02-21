package org.clulab.odin.debugger.odin

import org.clulab.odin.{Action, Mention, State}
import org.clulab.odin.debugger.Debugger
import org.clulab.odin.debugger.utils.RuleUtils
import org.clulab.odin.impl.{CrossSentenceExtractor, OdinException, Priority}
import org.clulab.processors.Document

class DebuggingCrossSentenceExtractor(
  override val debugger: Debugger,
  val crossSentenceExtractor: CrossSentenceExtractor,
  name: String,
  labels: Seq[String],
  priority: Priority,
  keep: Boolean,
  action: Action,
  leftWindow: Int,
  rightWindow: Int,
  debuggingAnchorExtractor: DebuggingTokenExtractor,
  debuggingNeighborExtractor: DebuggingTokenExtractor,
  anchorRole: String,
  neighborRole: String
) extends CrossSentenceExtractor(name, labels, priority, keep, action, leftWindow, rightWindow, debuggingAnchorExtractor,
    debuggingNeighborExtractor, anchorRole, neighborRole) with DebuggingExtractor {
  // Check for valid window values.
  if (leftWindow < 0) throw OdinException(s"left-window for '$name' must be >= 0")
  if (rightWindow < 0) throw OdinException(s"right-window for '$name' must be >= 0")

  def extractor: CrossSentenceExtractor = crossSentenceExtractor

  override def withDebuggingChildren: Seq[DebuggingExtractor] = Seq(this, debuggingAnchorExtractor, debuggingNeighborExtractor)

  // This comes indirectly through Extractor.
  override def findAllIn(doc: Document, state: State): Seq[Mention] = debugger.debugExtractor(crossSentenceExtractor) {
    super.findAllIn(doc, state)
  }

  def getMatchingMentionsFromState(state: State, mention: Mention): Seq[Mention] = {
    state.mentionsFor(mention.sentence, mention.tokenInterval).filter { stateMention =>
      // The span should match exactly.
      (stateMention.tokenInterval == mention.tokenInterval) &&
        // The label should match.
        (stateMention matches mention.label)
    }
  }

  override def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = debugger.debugSentence(sent, doc.sentences(sent)) {
    val labelAnchorMentions = debugger.debugAnchor(debuggingAnchorExtractor) {
      debugger.debugExtractor(debuggingAnchorExtractor) {
        debugger.debugTokenPattern(debuggingAnchorExtractor.pattern) {
          val allAnchorMentions = debuggingAnchorExtractor.findAllIn(sent, doc, state)

          // Find the mentions in the state that match the given span and label.
          allAnchorMentions.flatMap { mention => getMatchingMentionsFromState(state, mention) }
        }
      }
    }

    if (labelAnchorMentions.isEmpty) Nil // The rule failed.
    else debugger.debugNeighbor(debuggingNeighborExtractor) {
      val windowRange = Range(math.max(sent - leftWindow, 0), math.min(sent + rightWindow, doc.sentences.length))
      val labelNeighborMentions = windowRange.flatMap { i =>
        if (i == sent) Seq.empty // The neighbor cannot be in the same sentence as the anchor.
        else {
          debugger.debugSentence(i, doc.sentences(i)) {
            debugger.debugExtractor(debuggingNeighborExtractor) {
              debugger.debugTokenPattern(debuggingNeighborExtractor.pattern) {
                val allNeighborMentions = debuggingNeighborExtractor.findAllIn(i, doc, state)

                // Find the mentions in the state that match the given span and label.
                allNeighborMentions.flatMap { mention =>
                  getMatchingMentionsFromState(state, mention)
                }
              }
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
    val ruleOpt = crossSentenceExtractor.ruleOpt
    val (anchorRuleOpt, neighborRuleOpt) = (None, None) // ruleOpt.map { rule =>
     /* val map = RuleUtils.toMap(rule)
      val patternOpt = map.get("pattern") // Now find the trigger?

      // check for
      //     anaphor: Entity = [lemma="he"]
      //    antecedent: Enti
      println(patternOpt)

      (Some("a"), Some("b"))
    }.getOrElse((None, None))*/

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
      DebuggingTokenExtractor(debugger, crossSentenceExtractor.anchorPattern, anchorRuleOpt),
      DebuggingTokenExtractor(debugger, crossSentenceExtractor.neighborPattern, neighborRuleOpt),
      crossSentenceExtractor.anchorRole,
      crossSentenceExtractor.neighborRole
    )
  }
}
