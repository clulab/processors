package org.clulab.odin.debugger.debugging

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.{Action, EventMention, Mention, RelationMention, State, TextBoundMention, mkTokenInterval}
import org.clulab.odin.impl.{ArgumentPattern, GraphExtractor, GraphPattern, OdinConfig, Priority, RelationGraphPattern, TokenPattern, TriggerMentionGraphPattern, TriggerPatternGraphPattern}
import org.clulab.processors.Document
import org.clulab.struct.Interval

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
    // super.getMentions(sent, doc, state, labels, keep, ruleName)
    debugger.debugTrigger(trigger) {
      debugger.debugTokenPattern(trigger) {
        val tokenPatternResults = trigger.findAllIn(sent, doc, state)
        val eventMentions = tokenPatternResults.flatMap { tokenPatternResult =>
          val tokenInterval = Interval(tokenPatternResult.start, tokenPatternResult.end)
          val eventMentions = debugger.debugTokenInterval(tokenInterval) {
            val tbmTrigger = new TextBoundMention(labels, tokenInterval, sent, doc, keep, ruleName)
            val argsAndPathsSeq = extractArguments(tokenInterval, sent, doc, state)
            val eventMentions = argsAndPathsSeq.map { case (args, paths) =>
              new EventMention(labels, mkTokenInterval(tbmTrigger, args), tbmTrigger, args, paths, sent, doc, keep, ruleName)
            }
            val matches = eventMentions.nonEmpty

//            debugger.debugMatches(matches) // TODO: This could be debug pair or something at the end
            eventMentions
          }

          eventMentions
        }

        eventMentions
      }

//      for {
//        r <- trigger.findAllIn(sent, doc, state)
//        trig = new TextBoundMention(labels, Interval(r.start, r.end), sent, doc, keep, ruleName)
//        (args, paths) <- extractArguments(trig.tokenInterval, sent, doc, state)
//      } yield new EventMention(labels, mkTokenInterval(trig, args), trig, args, paths, sent, doc, keep, ruleName)
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
    // super.getMentions(sent, doc, state, labels, keep, ruleName)
    // TODO: record each mention and whether matched or not
    // debugger.debugMention
    // debugger.matches
    for {
      mention <- state.mentionsFor(sent)
      if mention matches triggerLabel
      if mention.isInstanceOf[TextBoundMention]
      trig = mention.asInstanceOf[TextBoundMention]
      (args, paths) <- extractArguments(trig.tokenInterval, sent, doc, state)
    } yield new EventMention(labels, mkTokenInterval(trig, args), trig, args, paths, sent, doc, keep, ruleName)
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
    // super.getMentions(sent, doc, state, labels, keep, ruleName)
    // TODO: record each mention and whether matched or not
    // debugger.debugMention
    // debugger.matches
    for {
      mention <- state.mentionsFor(sent)
      if mention matches anchorLabel
      (args, paths) <- extractArguments(mention.tokenInterval, sent, doc, state)
      relationArgs = args + (anchorName -> Seq(mention))
      relationPaths = paths + (anchorName -> Map(mention -> Nil))
    } yield new RelationMention(labels, mkTokenInterval(relationArgs), relationArgs, relationPaths, sent, doc, keep, ruleName)
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
  config: OdinConfig,
  ruleOpt: Option[String]
) extends GraphExtractor(name, labels, priority, keep, action, pattern, config, ruleOpt) {

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
    val debuggingGraphPattern = graphExtractor.pattern match {
      case graphPattern: TriggerPatternGraphPattern => DebuggingTriggerPatternGraphPattern(debugger, graphPattern)
      case graphPattern: TriggerMentionGraphPattern => DebuggingTriggerMentionGraphPattern(debugger, graphPattern)
      case graphPattern: RelationGraphPattern => DebuggingRelationGraphPattern(debugger, graphPattern)
    }

    new DebuggingGraphExtractor(
      debugger,
      graphExtractor,
      graphExtractor.name,
      graphExtractor.labels,
      graphExtractor.priority,
      graphExtractor.keep,
      graphExtractor.action,
      debuggingGraphPattern,
      graphExtractor.config,
      graphExtractor.ruleOpt
    )
  }
}
