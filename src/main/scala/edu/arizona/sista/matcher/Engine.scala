package edu.arizona.sista.matcher

import scala.collection.mutable.HashMap
import edu.arizona.sista.processors.{Document, Sentence}

class ExtractorEngine[T <: Actions](val spec: String, val actions: T) {
  val reader = new RuleReader(actions)
  val extractors = reader.read(spec)

  // the minimum number of iterations required for every rule to run at least once
  val minIterations = extractors.map(_.startsAt).max

  def extractFrom(document: Document) = {
    val state = new State

    var updated = true
    var iter = 0

    while (updated || iter < minIterations) {
      iter += 1
      updated = false
      for (extractor <- extractors if extractor.priority matches iter) {
        val mentions = extractor.findAllIn(document, state)
        if (mentions.nonEmpty) {
          state.update(mentions)
          updated = true
        }
      }
    }

    state.allMentions
  }
}
