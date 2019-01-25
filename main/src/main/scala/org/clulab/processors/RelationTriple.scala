package org.clulab.processors

import org.clulab.struct.Interval

case class RelationTriple(confidence:Float,
                          subjectInterval:Interval,
                          relationInterval:Interval,
                          objectInterval:Interval) {

  def subjectWords(implicit sentence:Sentence):Seq[String] = subjectInterval.map(sentence.words)
  def subjectLemmas(implicit sentence:Sentence):Seq[String] = subjectInterval.map(sentence.lemmas.get)
  def subjectText(implicit sentence:Sentence):String = subjectWords(sentence).mkString(" ")

  def relationWords(implicit sentence:Sentence):Seq[String] = relationInterval.map(sentence.words)
  def relationLemmas(implicit sentence:Sentence):Seq[String] = relationInterval.map(sentence.lemmas.get)
  def relationText(implicit sentence:Sentence):String = relationWords(sentence).mkString(" ")

  def objectWords(implicit sentence:Sentence):Seq[String] = objectInterval.map(sentence.words)
  def objectLemmas(implicit sentence:Sentence):Seq[String] = objectInterval.map(sentence.lemmas.get)
  def objectText(implicit sentence:Sentence):String = objectWords(sentence).mkString(" ")
}

