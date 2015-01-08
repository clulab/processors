package edu.arizona.sista.matcher

import scala.reflect.ClassTag
import scala.collection.mutable.HashMap
import edu.arizona.sista.processors.{Document, Sentence}

class ExtractorEngine[T <: Actions : ClassTag](val spec: String, val actions: T, postprocess: Seq[Mention] => Seq[Mention]) {
  def this(spec: String, actions: T) = this(spec, actions, identity)

  val reader = new RuleReader(actions)
  val extractors = reader.read(spec)

  // the minimum number of iterations required for every rule to run at least once
  val minIterations = extractors.map(_.startsAt).max

  def extractFrom(document: Document): Seq[Mention] = {
    @annotation.tailrec
    def loop(i: Int, state: State): Seq[Mention] = iteration(i, state) match {
      case Nil if minIterations <= i => state.allMentions
      case Nil => loop(i + 1, state)
      case mentions => loop(i + 1, state.update(postprocess(mentions)))
    }

    def iteration(i: Int, state: State): Seq[Mention] = for {
      extractor <- extractors
      if extractor.priority matches i
      mention <- extractor.findAllIn(document, state)
    } yield mention

    loop(1, new State)
  }
}
