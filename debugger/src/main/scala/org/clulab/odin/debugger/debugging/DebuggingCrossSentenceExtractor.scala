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

  // This comes indirectly through Extractor.
  override def findAllIn(doc: Document, state: State): Seq[Mention] = debugger.debugExtractor(crossSentenceExtractor) {
    super.findAllIn(doc, state)
  }

  override def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = debugger.debugSentence(sent, doc.sentences(sent)) {
    // super.findAllIn(sent, doc, state) // TODO. This was copied from super and needs to have debugging added.

    def getMentionsWithLabel(m: Mention): Seq[Mention] = {
      state.mentionsFor(m.sentence, m.tokenInterval).filter{ mention =>
        // the span should match exactly
        (mention.tokenInterval == m.tokenInterval) &&
        // the label should match
        (mention matches m.label)
      }
    }

    anchorPattern.findAllIn(sent, doc, state) match {
      // the rule failed
      case Nil => Nil
      // the anchor matched something
      case anchorMentions =>

        // check for valid window values
        if (leftWindow < 0)  throw OdinException(s"left-window for '$name' must be >= 0")
        if (rightWindow < 0) throw OdinException(s"right-window for '$name' must be >= 0")

        val mentions = for {
          i <- sent - leftWindow to sent + rightWindow
          // is the sentence within the allotted window?
          if 0 <= i && i < doc.sentences.length
          // the neighbor cannot be in the same sentence as the anchor
          if i != sent
          // find the mentions in the state that match the given span and label
          anchor <- anchorMentions.flatMap(getMentionsWithLabel)
          //_ = println(s"Anchor:${anchor.labels}: '${anchor.text}' foundBy ${anchor.foundBy}")
          // attempt to match the neighbor's pattern
          neighbor <- neighborPattern.findAllIn(i, doc, state).flatMap(getMentionsWithLabel)
          //_ = println(s"Neighbor:${neighbor.labels}: '${neighbor.text}' foundBy ${neighbor.foundBy}")
          // the anchor and neighbor cannot be in the same sentence
          // if anchor.sentence != neighbor.sentence
        } yield mkMention(anchor, neighbor)

        action(mentions, state)
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
