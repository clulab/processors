package org.clulab.odin.impl

import org.clulab.struct.Interval
import org.clulab.processors.Document
import org.clulab.odin._

trait GraphPattern {
  def arguments: Seq[ArgumentPattern]

  // separate the required and optional arguments
  protected val (required, optional) = arguments.partition(_.required)

  type Args = Map[String, Seq[Mention]]
  type Paths = Map[String, Map[Mention, SynPath]]
  val config: OdinConfig

  def getMentions(
      sent: Int,
      doc: Document,
      state: State,
      labels: Seq[String],
      keep: Boolean,
      ruleName: String
  ): Seq[Mention]

  protected def extractArguments(
      tokens: Interval,
      sent: Int,
      doc: Document,
      state: State
  ): Seq[(Args, Paths)] = {

    // extract required arguments
    val reqExtractions = extractArguments(required, tokens, sent, doc, state)

    // if not all required arguments were found, return Nil
    val reqNames = required.map(_.name)
    val foundAllRequired = reqNames.forall(reqExtractions.contains)
    if (!foundAllRequired) return Nil

    // get the arguments out of the extraction
    // while preserving the extraction groups
    val reqArgs: Seq[Seq[(String, Seq[Mention])]] = for {
      (name, mentionsWithPathsGroups) <- reqExtractions.toSeq
    } yield mentionsWithPathsGroups.map(g => name -> g.map(_._1))

    // get the paths, resulting in an unserializable MapLike
    val reqPaths = reqExtractions.mapValues(_.flatten.toMap)

    // extract optional arguments
    val optExtractions = extractArguments(optional, tokens, sent, doc, state)

    // get the arguments out of the extraction
    val optArgs: Seq[Seq[(String, Seq[Mention])]] = for {
      (name, mentionsWithPathsGroups) <- optExtractions.toSeq
    } yield mentionsWithPathsGroups.map(g => name -> g.map(_._1))

    // get the paths, resulting in an unserializable MapLike
    val optPaths = optExtractions.mapValues(_.flatten.toMap)

    // group the paths together, ensuring the result is a serializable Map
    val paths: Paths = Map.empty ++ reqPaths ++ optPaths
    // group the arguments together
    val args: Seq[Seq[(String, Seq[Mention])]] = reqArgs ++ optArgs
    // return cartesian product of arguments
    product(args).map(a => (a.toMap, paths))

  }

  // Extracts the given arguments from any of the tokens in the interval.
  // Recall that each argument has arity, and their extractions are grouped
  // according to this arity. The extraction is represented as Seq[Seq[(Mention, SynPath)]]]
  // containing a sequence of extracted groups, each group has one or more (mention, syntactic path) tuples.
  // This function returns a map from argument name to extraction.
  private def extractArguments(
      arguments: Seq[ArgumentPattern],
      tokens: Interval,
      sent: Int,
      doc: Document,
      state: State
  ): Map[String, Seq[Seq[(Mention, SynPath)]]] = {
    val extractions = for {
      a <- arguments
      t <- tokens
      results = a.extract(t, sent, doc, state)
      if results.nonEmpty
    } yield (a.name -> results)
    extractions.toMap
  }

  // cartesian product
  // from: List(List(x1, x2, x3), List(y1, y2))
  // to: List(List(x1, y1), List(x1, y2), List(x2, y1), List(x2, y2), List(x3, y1), List(x3, y2))
  private def product[A](xss: Seq[Seq[A]]) = xss.foldRight(Seq(Seq[A]())) {
    (xs, lla) => xs.flatMap(x => lla.map(x +: _))
  }
}

// creates an EventMention using a TokenPattern for the trigger
class TriggerPatternGraphPattern(
    val trigger: TokenPattern,
    val arguments: Seq[ArgumentPattern],
    val config: OdinConfig
) extends GraphPattern {
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
class TriggerMentionGraphPattern(
    val triggerLabel: String,
    val arguments: Seq[ArgumentPattern],
    val config: OdinConfig
) extends GraphPattern {
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
class RelationGraphPattern(
    val anchorName: String,
    val anchorLabel: String,
    val arguments: Seq[ArgumentPattern],
    val config: OdinConfig
) extends GraphPattern {
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
