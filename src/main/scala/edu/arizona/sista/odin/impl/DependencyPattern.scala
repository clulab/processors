package edu.arizona.sista.odin.impl

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin._

class DependencyPattern(val trigger: TokenPattern, val arguments: Seq[ArgumentPattern]) {
  // the labels of the required arguments
  private val required = for (a <- arguments if a.required) yield a
  private val optional = for (a <- arguments if !a.required) yield a

  type Args = Map[String, Seq[Mention]]

  def getMentions(
    sent: Int,
    doc: Document,
    state: State,
    label: String,
    keep: Boolean,
    ruleName: String
  ): Seq[Mention] = for {
    r <- trigger.findAllIn(sent, doc, state)
    trig = new TextBoundMention(label, Interval(r.start, r.end), sent, doc, keep, ruleName)
    args <- extractArguments(trig.tokenInterval, sent, doc, state)
  } yield new EventMention(label, trig, args, sent, doc, keep, ruleName)

  // extract the arguments of a trigger represented as a token interval
  private def extractArguments(
    trig: Interval,
    sent: Int,
    doc: Document,
    state: State
  ): Seq[Args] = for {
    tok <- trig.toSeq
    m <- extractArguments(tok, sent, doc, state)
  } yield m

  // extract arguments for one of the tokens in the trigger
  private def extractArguments(tok: Int, sent: Int, doc: Document, state: State): Seq[Args] = {
    val req = for (a <- required) yield {
      val results = a.extract(tok, sent, doc, state)
      if (results.isEmpty) return Nil
      results.map(a.name -> _)
    }
    val opt = for (a <- optional) yield a.extract(tok, sent, doc, state).map(a.name -> _)
    val args = product(req ++ opt) map (_.toMap)
    args
  }

  // cartesian product
  private def product[A](xss: Seq[Seq[A]]) = xss.foldRight(Seq(Seq[A]())) {
    (xs, lla) => xs.flatMap(x => lla.map(x +: _))
  }

  private def mergeMatches(lhs: Args, rhs: Args): Args = {
    val keys = lhs.keySet ++ rhs.keySet
    val args = keys map (k => (k -> (lhs.getOrElse(k, Nil) ++ rhs.getOrElse(k, Nil))))
    args.toMap
  }
}

object DependencyPattern {
  def compile(input: String): DependencyPattern = DependencyPatternCompiler.compile(input)
}

object DependencyPatternCompiler extends DependencyPatternParsers {
  def compile(input: String): DependencyPattern =
    parseAll(dependencyPattern, clean(input)) match {
      case Success(result, _) => result
      case failure: NoSuccess => sys.error(failure.msg)
    }

  // remove commented lines and trim whitespaces
  def clean(input: String): String = input.replaceAll("""(?m)^\s*#.*$""", "").trim
}
