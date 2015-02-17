package edu.arizona.sista.odin.impl

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin._

class DependencyPattern(val trigger: TokenPattern, val arguments: Seq[ArgumentPattern]) {
  // the labels of the required arguments
  private val required = for (a <- arguments if a.required) yield a.name

  type Match = Map[String, Seq[Interval]]

  def findAllIn(doc: Document): Seq[(Match, Int)] =
    findAllIn(doc, None)

  def findAllIn(doc: Document, state: State): Seq[(Match, Int)] =
    findAllIn(doc, Some(state))

  // returns matches with sentence id
  def findAllIn(doc: Document, state: Option[State]): Seq[(Match, Int)] = for {
    i <- 0 until doc.sentences.size
    m <- findAllIn(i, doc, state)
  } yield (m, i)

  def findAllIn(sent: Int, doc: Document): Seq[Match] =
    findAllIn(sent, doc, None)

  def findAllIn(sent: Int, doc: Document, state: State): Seq[Match] =
    findAllIn(sent, doc, Some(state))

  def findAllIn(sent: Int, doc: Document, state: Option[State]): Seq[Match] =
    for {
      r <- trigger.findAllIn(sent, doc, state)
      trig = Interval(r.start, r.end)
      args <- extractArguments(trig, sent, doc, state)
    } yield args + ("trigger" -> Seq(trig))

  // extract the arguments of a trigger represented as a token interval
  private def extractArguments(trig: Interval, sent: Int, doc: Document, state: Option[State]): Option[Match] = {
    val matches = trig.toSeq.map(tok => extractArguments(tok, sent, doc, state))
    if (matches.isEmpty) None
    else {
      val result = matches reduce mergeMatches
      if (required exists (arg => result.getOrElse(arg, Nil).isEmpty)) None
      else Some(result)
    }
  }

  // extract arguments for one of the tokens in the trigger
  private def extractArguments(tok: Int, sent: Int, doc: Document, state: Option[State]): Match =
    arguments.map(a => (a.name -> a.findAllIn(tok, sent, doc, state))).toMap

  private def mergeMatches(lhs: Match, rhs: Match): Match = {
    val keys = lhs.keySet ++ rhs.keySet
    val args = keys map (k => (k -> (lhs.getOrElse(k, Nil) ++ rhs.getOrElse(k, Nil))))
    args.toMap
  }
}

object DependencyPattern {
  def compile(input: String): DependencyPattern = DependencyPatternCompiler.compile(input)
}

object DependencyPatternCompiler extends DependencyPatternParsers {
  def compile(input: String): DependencyPattern = parseAll(dependencyPattern, clean(input)) match {
    case Success(result, _) => result
    case failure: NoSuccess => sys.error(failure.msg)
  }

  // remove commented lines and trim whitespaces
  def clean(input: String): String = input.replaceAll("""(?m)^\s*#.*$""", "").trim
}
