package edu.arizona.sista.matcher

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document

class DependencyPattern(val trigger: TokenPattern, val arguments: Seq[ArgumentPattern]) {
  // the labels of the required arguments
  private val required = for (a <- arguments if a.required) yield a.name

  type Match = Map[String, Seq[Interval]]

  // returns matches with sentence id
  def findAllIn(doc: Document): Seq[(Match, Int)] = for {
    i <- 0 until doc.sentences.size
    m <- findAllIn(i, doc)
  } yield (m, i)

  def findAllIn(sent: Int, doc: Document): Seq[Match] =
    for {
      r <- trigger.findAllIn(sent, doc)
      trig = Interval(r.start, r.end)
      args <- extractArguments(trig, sent, doc)
    } yield args + ("trigger" -> Seq(trig))

  // extract the arguments of a trigger represented as a token interval
  private def extractArguments(trig: Interval, sent: Int, doc: Document): Option[Match] = {
    val matches = trig.toRange.map(tok => extractArguments(tok, sent, doc))
    if (matches.isEmpty) None
    else {
      val result = matches reduce mergeMatches
      if (required exists (arg => result.getOrElse(arg, Nil).isEmpty)) None
      else Some(result)
    }
  }

  // extract arguments for one of the tokens in the trigger
  private def extractArguments(tok: Int, sent: Int, doc: Document): Match =
    arguments.map(a => (a.name -> a.findAllIn(tok, sent, doc))).toMap

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
  def compile(input: String): DependencyPattern = parseAll(dependencyPattern, input.trim) match {
    case Success(result, _) => result
    case failure: NoSuccess => sys.error(failure.msg)
  }
}
