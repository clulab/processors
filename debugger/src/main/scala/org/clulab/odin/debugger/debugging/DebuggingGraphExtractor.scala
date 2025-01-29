package org.clulab.odin.debugger.debugging

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.{Action, Mention, State}
import org.clulab.odin.impl.{ArgumentPattern, GraphExtractor, GraphPattern, OdinConfig, Priority, RelationGraphPattern, TokenPattern, TriggerMentionGraphPattern, TriggerPatternGraphPattern}
import org.clulab.processors.Document

class DebuggingTriggerPatternGraphPattern(
  val debugger: Debugger,
  trigger: TokenPattern,
  arguments: Seq[ArgumentPattern], // TODO
  config: OdinConfig
) extends TriggerPatternGraphPattern(trigger, arguments, config) {

  override def getMentions(
    sent: Int,
    doc: Document,
    state: State,
    labels: Seq[String],
    keep: Boolean,
    ruleName: String
  ): Seq[Mention] = {
    super.getMentions(sent, doc, state, labels, keep, ruleName)
    // TODO: record should be made automatically
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

class DebuggingTriggerMentionGraphPattern(
  val debugger: Debugger,
  triggerLabel: String,
  arguments: Seq[ArgumentPattern], // TODO
  config: OdinConfig
) extends TriggerMentionGraphPattern(triggerLabel, arguments, config) {

  override def getMentions(
    sent: Int,
    doc: Document,
    state: State,
    labels: Seq[String],
    keep: Boolean,
    ruleName: String
  ): Seq[Mention] = {
    super.getMentions(sent, doc, state, labels, keep, ruleName)
    // TODO: record each mention and whether matched or not
    // debugger.debugMention
    // debugger.matches
  }
}

object DebuggingTriggerMentionGraphPattern {

  def apply(debugger: Debugger, triggerMentionGraphPattern: TriggerMentionGraphPattern) = {
    new DebuggingTriggerMentionGraphPattern(
      debugger,
      triggerMentionGraphPattern.triggerLabel,
      triggerMentionGraphPattern.arguments,
      triggerMentionGraphPattern.config
    )
  }
}

class DebuggingRelationGraphPattern(
  val debugger: Debugger,
  anchorName: String,
  anchorLabel: String,
  arguments: Seq[ArgumentPattern],  // TODO
  config: OdinConfig
) extends RelationGraphPattern(anchorName, anchorLabel, arguments, config) {

    override def getMentions(
    sent: Int,
    doc: Document,
    state: State,
    labels: Seq[String],
    keep: Boolean,
    ruleName: String
  ): Seq[Mention] = {
    super.getMentions(sent, doc, state, labels, keep, ruleName)
    // TODO: record each mention and whether matched or not
    // debugger.debugMention
    // debugger.matches
  }
}

object DebuggingRelationGraphPattern {

  def apply(debugger: Debugger, relationGraphPattern: RelationGraphPattern): DebuggingRelationGraphPattern = {
    new DebuggingRelationGraphPattern(
      debugger,
      relationGraphPattern.anchorName,
      relationGraphPattern.anchorLabel,
      relationGraphPattern.arguments, // TODO, debugging version?
      relationGraphPattern.config
    )
  }
}



class DebuggingGraphExtractor(
  val debugger: Debugger,
  val graphExtractor: GraphExtractor,
  name: String,
  labels: Seq[String],
  priority: Priority,
  keep: Boolean,
  action: Action,
  pattern: GraphPattern,
  config: OdinConfig
) extends GraphExtractor(name, labels, priority, keep, action, pattern, config) {

  // This comes indirectly through Extractor.
  override def findAllIn(doc: Document, state: State): Seq[Mention] = debugger.debugExtractor(graphExtractor) {
    super.findAllIn(doc, state)
  }

  override def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = debugger.debugSentence(sent, doc.sentences(sent)) {
    super.findAllIn(sent, doc, state)
  }
}

object DebuggingGraphExtractor {

  def apply(debugger: Debugger, graphExtractor: GraphExtractor): DebuggingGraphExtractor = {
    new DebuggingGraphExtractor(
      debugger,
      graphExtractor,
      graphExtractor.name,
      graphExtractor.labels,
      graphExtractor.priority,
      graphExtractor.keep,
      graphExtractor.action,
      graphExtractor.pattern,
      graphExtractor.config
    )
  }
}
