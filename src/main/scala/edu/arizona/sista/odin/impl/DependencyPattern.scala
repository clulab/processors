package edu.arizona.sista.odin.impl

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin._

trait DependencyPattern {
  def arguments: Seq[ArgumentPattern]

  // separate the required and optional arguments
  protected val (required, optional) = arguments.partition(_.required)

  type Args = Map[String, Seq[Mention]]

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
  ): Seq[Args] = for {
    tok <- interval.toSeq
    m <- extractArguments(tok, sent, doc, state)
  } yield m

  // extract arguments for one of the tokens in the trigger
  private def extractArguments(tok: Int, sent: Int, doc: Document, state: State): Seq[Args] = {
    // extract all required arguments
    val req = for (a <- required) yield {
      val results = a.extract(tok, sent, doc, state)
      // if a required argument is not present stop extraction
      if (results.isEmpty) return Nil
      results.map(a.name -> _)
    }
    // extract optional arguments
    val opt = for (a <- optional) yield a.extract(tok, sent, doc, state).map(a.name -> _)
    // drop empty optional arguments
    val args = req ++ opt.filter(_.nonEmpty)
    // return cartesian product of arguments
    product(args).map(_.toMap)
  }

  // cartesian product
  // from: List(List(x1, x2, x3), List(y1, y2))
  // to: List(List(x1, y1), List(x1, y2), List(x2, y1), List(x2, y2), List(x3, y1), List(x3, y2))
  private def product[A](xss: Seq[Seq[A]]) = xss.foldRight(Seq(Seq[A]())) {
    (xs, lla) => xs.flatMap(x => lla.map(x +: _))
  }
}

// if we have a trigger then we create an EventMention
class EventDependencyPattern(
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
    args <- extractArguments(trig.tokenInterval, sent, doc, state)
  } yield new EventMention(labels, trig, args, sent, doc, keep, ruleName)
}

// if a trigger wasn't specified then we create a RelationMention
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
    tok <- 0 until doc.sentences(sent).size
    mention <- state.mentionsFor(sent, tok, anchorLabel)
    args <- extractArguments(mention.tokenInterval, sent, doc, state)
    relationArgs = args + (anchorName -> Seq(mention))
  } yield new RelationMention(labels, relationArgs, sent, doc, keep, ruleName)
}

object DependencyPattern {
  def compile(input: String): DependencyPattern = DependencyPatternCompiler.compile(input)
}
