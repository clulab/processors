package org.clulab.odin.impl

import org.clulab.struct.Interval
import org.clulab.processors.Document
import org.clulab.odin._

trait DependencyPattern {
  def arguments: Seq[ArgumentPattern]

  // separate the required and optional arguments
  protected val (required, optional) = arguments.partition(_.required)

  type Args = Map[String, Seq[Mention]]
  type Paths = Map[String, Map[Mention, SynPath]]

  def getMentions(
      sent: Int,
      doc: Document,
      state: State,
      labels: Seq[String],
      keep: Boolean,
      ruleName: String
  ): Seq[Mention]

  // extract the arguments of a token interval
  protected def extractArguments(
      interval: Interval,
      sent: Int,
      doc: Document,
      state: State
  ): Seq[(Args, Paths)] = for {
    tok <- interval
    mp <- extractArguments(tok, sent, doc, state)
  } yield mp

  // extract arguments for one of the tokens in the trigger
  private def extractArguments(
      tok: Int,
      sent: Int,
      doc: Document,
      state: State
  ): Seq[(Args, Paths)] = {
    // variable to collect paths
    var paths: Map[String, Map[Mention, SynPath]] = Map.empty
    // extract all required arguments
    val req = for (a <- required) yield {
      val results: Seq[Seq[(Mention, SynPath)]] = a.extract(tok, sent, doc, state)
      // if a required argument is not present stop extraction
      if (results.isEmpty) return Nil
      paths += (a.name -> results.flatten.toMap) // collect paths
      results.map(a.name -> _.map(_._1))
    }
    // extract optional arguments
    val opt = for (a <- optional) yield {
      val results: Seq[Seq[(Mention, SynPath)]] = a.extract(tok, sent, doc, state)
      paths += (a.name -> results.flatten.toMap) // collect paths
      results.map(a.name -> _.map(_._1))
    }
    // drop empty optional arguments
    val args = req ++ opt.filter(_.nonEmpty)
    // return cartesian product of arguments
    product(args).map(a => (a.toMap, paths))
  }

  // cartesian product
  // from: List(List(x1, x2, x3), List(y1, y2))
  // to: List(List(x1, y1), List(x1, y2), List(x2, y1), List(x2, y2), List(x3, y1), List(x3, y2))
  private def product[A](xss: Seq[Seq[A]]) = xss.foldRight(Seq(Seq[A]())) {
    (xs, lla) => xs.flatMap(x => lla.map(x +: _))
  }
}

// creates an EventMention using a TokenPattern for the trigger
class TriggerPatternDependencyPattern(
    val trigger: TokenPattern,
    val arguments: Seq[ArgumentPattern]
) extends DependencyPattern {
  def getMentions(
      sent: Int,
      doc: Document,
      state: State,
      labels: Seq[String],
      keep: Boolean,
      ruleName: String
  ): Seq[Mention] = for {
    r <- trigger.findAllIn(sent, doc, state)
    trig = new TextBoundMention(labels, Interval(r.start, r.end), sent, doc, keep, ruleName)
    (args, paths) <- extractArguments(trig.tokenInterval, sent, doc, state)
  } yield new EventMention(labels, mkTokenInterval(trig, args), trig, args, paths, sent, doc, keep, ruleName)
}

// creates an EventMention by matching trigger mentions
class TriggerMentionDependencyPattern(
    val triggerLabel: String,
    val arguments: Seq[ArgumentPattern]
) extends DependencyPattern {
  def getMentions(
      sent: Int,
      doc: Document,
      state: State,
      labels: Seq[String],
      keep: Boolean,
      ruleName: String
  ): Seq[Mention] = for {
    mention <- state.mentionsFor(sent)
    if mention matches triggerLabel
    if mention.isInstanceOf[TextBoundMention]
    trig = mention.asInstanceOf[TextBoundMention]
    (args, paths) <- extractArguments(trig.tokenInterval, sent, doc, state)
  } yield new EventMention(labels, mkTokenInterval(trig, args), trig, args, paths, sent, doc, keep, ruleName)
}

// creates a RelationMention by matching mentions
class RelationDependencyPattern(
    val anchorName: String,
    val anchorLabel: String,
    val arguments: Seq[ArgumentPattern]
) extends DependencyPattern {
  def getMentions(
      sent: Int,
      doc: Document,
      state: State,
      labels: Seq[String],
      keep: Boolean,
      ruleName: String
  ): Seq[Mention] = for {
    mention <- state.mentionsFor(sent)
    if mention matches anchorLabel
    (args, paths) <- extractArguments(mention.tokenInterval, sent, doc, state)
    relationArgs = args + (anchorName -> Seq(mention))
    relationPaths = paths + (anchorName -> Map(mention -> Nil))
  } yield new RelationMention(labels, mkTokenInterval(relationArgs), relationArgs, relationPaths, sent, doc, keep, ruleName)
}
