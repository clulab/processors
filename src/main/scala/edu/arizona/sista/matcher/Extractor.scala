package edu.arizona.sista.matcher

import edu.arizona.sista.processors.{Document, Sentence}

trait Extractor {
  def findAllIn(sentence: Sentence, state: State, ruleName: String): Seq[Map[String, Seq[Int]]]
}

class NamedExtractor(val name: String, val priority: Priority, val extractor: Extractor, val action: Action) {
  def extractFrom(document: Document, state: State): Seq[Mention] = {
    val mentions = document.sentences.zipWithIndex flatMap {
      case (sentence, i) => extractor.findAllIn(sentence, state, name) flatMap {
        x => action(i, state, x)
      }
    }
    // remember who found this mention
    mentions foreach (_.foundBy = Some(name))
    mentions
  }

  def startsAt: Int = priority match {
    case ExactPriority(i) => i
    case IntervalPriority(start, end) => start
    case FromPriority(from) => from
  }
}
