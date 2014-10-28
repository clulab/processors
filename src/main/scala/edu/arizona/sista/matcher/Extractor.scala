package edu.arizona.sista.matcher

import edu.arizona.sista.processors.{Document, Sentence}

trait Extractor {
  def findAllIn(sentence: Sentence, state: State): Seq[Map[String, Seq[Int]]]
}

class NamedExtractor(val name: String, val priority: Priority, val extractor: Extractor, val action: Action) {
  def extractFrom(document: Document, state: State): Seq[Mention] = {
    document.sentences.zipWithIndex flatMap {
      case (sentence, i) => extractor.findAllIn(sentence, state) flatMap {
        x => action(i, state, x)
      }
    }
  }

  def startsAt: Int = priority match {
    case ExactPriority(i) => i
    case IntervalPriority(start, end) => start
    case FromPriority(from) => from
  }
}
