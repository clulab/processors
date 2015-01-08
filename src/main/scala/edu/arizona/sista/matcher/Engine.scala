package edu.arizona.sista.matcher

import scala.reflect.ClassTag
import edu.arizona.sista.processors.{Document, Sentence}

class ExtractorEngine[T <: Actions : ClassTag](rules: String, actions: T, postprocess: PostProcessor = identity) {
  val reader = new RuleReader(actions)
  val extractors = reader.read(rules)

  // the minimum number of iterations required for every rule to run at least once
  val minIterations = extractors.map(_.startsAt).max

  def extractFrom(document: Document): Seq[Mention] = {
    @annotation.tailrec
    def loop(i: Int, state: State): Seq[Mention] = iteration(i, state) match {
      case Nil if i >= minIterations => state.allMentions  // we are done
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
