package org.clulab.processors.clu

case class AnnotatedSentence(
  words: IndexedSeq[String],
  posTags: Option[IndexedSeq[String]] = None,
  neTags: Option[IndexedSeq[String]] = None,
  headPositions: Option[IndexedSeq[Int]] = None
) {
  def indices: Range = words.indices
  def size: Int = words.size
}
