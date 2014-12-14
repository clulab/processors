package edu.arizona.sista.matcher

import scala.collection.mutable.HashMap
import edu.arizona.sista.processors.{Document, Sentence}

class ExtractorEngine[T <: Actions](val spec: String, val actions: T) {
  val loader = new ExtractorLoader(actions)
  val extractors = loader.load(spec)

  // the minimum number of iterations required for every rule to run at least once
  val minIterations = extractors.map(_.startsAt).max

  def extractFrom(document: Document) = {
    val state = new State(document)

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
