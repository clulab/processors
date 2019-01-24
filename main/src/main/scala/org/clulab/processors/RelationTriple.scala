package org.clulab.processors

import org.clulab.struct.Interval

case class RelationTriple(doc:Document,
                          sentence:Int,
                          subjectInterval:Interval,
                          relationInterval:Interval,
                          objectInterval:Interval) {

  def sentenceObj:Sentence = doc.sentences(sentence)

  def subjectWords:Seq[String] = subjectInterval.map(sentenceObj.words)
  def subjectLemmas:Seq[String] = subjectInterval.map(sentenceObj.lemmas.get)
  def subjectText:String = subjectWords.mkString(" ")

  def relationWords:Seq[String] = relationInterval.map(sentenceObj.words)
  def relationLemmas:Seq[String] = relationInterval.map(sentenceObj.lemmas.get)
  def relationText:String = relationInterval.map(sentenceObj.words).mkString(" ")

  def objectWords:Seq[String] = objectInterval.map(sentenceObj.words)
  def objectLemmas:Seq[String] = objectInterval.map(sentenceObj.lemmas.get)
  def objectText:String = objectInterval.map(sentenceObj.words).mkString(" ")

  override def toString: String = s"$subjectText $relationText $objectText"
}
