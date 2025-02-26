package org.clulab.odin.debugger.odin

import org.clulab.odin.debugger.Debugger
import org.clulab.odin.debugger.debug.MentionMatch
import org.clulab.odin.{Action, EventMention, Mention, RelationMention, State, TextBoundMention, mkTokenInterval}
import org.clulab.odin.impl.{ArgumentPattern, GraphExtractor, GraphPattern, OdinConfig, Priority, RelationGraphPattern, TokenPattern, TriggerMentionGraphPattern, TriggerPatternGraphPattern}
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
  ): Seq[Mention] = {
    debugger.debugTrigger(trigger) {
      debugger.debugTokenPattern(trigger) {
        val tokenPatternResults = trigger.findAllIn(sent, doc, state)
        val eventMentions = tokenPatternResults.flatMap { tokenPatternResult =>
          val tokenInterval = Interval(tokenPatternResult.start, tokenPatternResult.end)
          val eventMentions = debugger.debugTokenInterval(tokenInterval) {
            lazy val tbmTrigger = new TextBoundMention(labels, tokenInterval, sent, doc, keep, ruleName)
            val arguments = extractArguments(tokenInterval, sent, doc, state)
            // Is this a good place to record mention comparisons?
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

class DebuggingTriggerMentionGraphPattern(
  val debugger: Debugger,
  triggerLabel: String,
  arguments: Seq[ArgumentPattern],
  config: OdinConfig
) extends TriggerMentionGraphPattern(triggerLabel, arguments, config) {
  val debuggingMention = DebuggingMention(triggerLabel)

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
  ): Seq[Mention] = {
    val allStateMentions = state.allMentions // Only get this once.
    val textBoundMentions = getMatchingMentionsFromState(state, sent, allStateMentions)
    val eventMentions = textBoundMentions.flatMap { textBoundMention =>
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
    val mentions1 = for {
      mention <- state.mentionsFor(sent)
      if mention matches anchorLabel
      (args, paths) <- extractArguments(mention.tokenInterval, sent, doc, state)
      relationArgs = args + (anchorName -> Seq(mention))
      relationPaths = paths + (anchorName -> Map(mention -> Nil))
    } yield new RelationMention(labels, mkTokenInterval(relationArgs), relationArgs, relationPaths, sent, doc, keep, ruleName)

    val mentions = state.mentionsFor(sent).flatMap { mention =>
      val matches = mention.matches(anchorLabel)

      // debugger.debugMentionMatches(mention, matches)
      if (matches) {
        val arguments = extractArguments(mention.tokenInterval, sent, doc, state)
        val relationMentions = arguments.map { case (args, paths) =>
          val relationArgs = args + (anchorName -> Seq(mention))
          val relationPaths = paths + (anchorName -> Map(mention -> Nil))

          new RelationMention(labels, mkTokenInterval(relationArgs), relationArgs, relationPaths, sent, doc, keep, ruleName)
        }

        relationMentions
      }
      else Seq.empty
    }

    if (mentions1.length != mentions.length)
      println("This is bad!")

    mentions
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
  debuggingAction: DebuggingAction,
  pattern: GraphPattern,
  config: OdinConfig,
  ruleOpt: Option[String]
) extends GraphExtractor(name, labels, priority, keep, debuggingAction, pattern, config, ruleOpt) with DebuggingExtractor {

  def extractor: GraphExtractor = graphExtractor

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
      DebuggingAction(debugger, graphExtractor.action, Some(graphExtractor)),
      DebuggingGraphPattern(debugger, graphExtractor.pattern),
      graphExtractor.config,
      graphExtractor.ruleOpt
    )
  }
}
