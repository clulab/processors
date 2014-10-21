package edu.arizona.sista.matcher

import edu.arizona.sista.processors.Sentence

trait Extractor {
  def findAllIn(sentence: Sentence): Seq[Map[String, Seq[Int]]]
}
