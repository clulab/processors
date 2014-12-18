package edu.arizona.sista.matcher

import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document

trait Extractor {
  def name: String
  def label: String
  def priority: Priority
  def action: Action

  def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention]

  def findAllIn(doc: Document, state: State): Seq[Mention] = for {
    i <- 0 until doc.sentences.size
    m <- findAllIn(i, doc, state)
  } yield m

  def startsAt: Int = priority match {
    case ExactPriority(i) => i
    case IntervalPriority(start, end) => start
    case FromPriority(from) => from
  }
}

class TokenExtractor(val name: String,
                     val label: String,
                     val priority: Priority,
                     val action: Action,
                     val pattern: TokenPattern) extends Extractor {

  def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = for {
    r <- pattern.findAllIn(sent, doc, state)
    m = Map("--GLOBAL--" -> Seq(r.interval)) ++ r.groups.mapValues(Seq(_))
    mention <- action(label, m, sent, doc, name, state)
    if !state.contains(mention)
  } yield mention
}

class DependencyExtractor(val name: String,
                          val label: String,
                          val priority: Priority,
                          val action: Action,
                          val pattern: DependencyPattern) extends Extractor {

  def findAllIn(sent: Int, doc: Document, state: State): Seq[Mention] = for {
    m <- pattern.findAllIn(sent, doc, state)
    mention <- action(label, m, sent, doc, name, state)
    if !state.contains(mention)
  } yield mention
}
