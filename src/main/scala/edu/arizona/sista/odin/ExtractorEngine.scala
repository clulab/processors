package edu.arizona.sista.odin

import scala.reflect.ClassTag
import edu.arizona.sista.processors.Document
import edu.arizona.sista.odin.impl.RuleReader

class ExtractorEngine[A <: Actions : ClassTag](
    rules: String,
    actions: A = new Actions,
    globalAction: Action = identityAction
) {
  val extractors = RuleReader(actions).read(rules)

  // the minimum number of iterations required for every rule to run at least once
  val minIterations = extractors.map(_.startsAt).max

  def extractFrom(document: Document, initialState: State = new State): Seq[Mention] = {
    @annotation.tailrec
    def loop(i: Int, state: State): Seq[Mention] = extract(i, state) match {
      case Nil if i >= minIterations => state.allMentions  // we are done
      case Nil => loop(i + 1, state)
      case mentions => loop(i + 1, state.updated(mentions))
    }

    def extract(i: Int, state: State): Seq[Mention] = {
      // extract mentions using extractors (each extractor applies its own action)
      val extractedMentions = for {
        extractor <- extractors
        if extractor.priority matches i
        mention <- extractor.findAllIn(document, state)
      } yield mention
      // apply globalAction to all extracted mentions
      val finalMentions = globalAction(extractedMentions, state)
      // only return mentions that are not already in the state
      finalMentions.filter(!state.contains(_))
    }

    loop(1, initialState)
  }
}
