package org.clulab.odin.debugger.debug

import org.clulab.odin.impl.{Extractor, TokenPattern}
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Interval

case class ImmutableDebuggerContext(
  depth: Int = 0,
  documents: List[Document] = List.empty,
  loops: List[Int] = List.empty,
  extractors: List[Extractor] = List.empty,
  tokenPatterns: List[TokenPattern] = List.empty,
  sentenceIndexes: List[Int] = List.empty,
  sentences: List[Sentence] = List.empty,
  starts: List[Int] = List.empty,
  toks: List[Int] = List.empty,
  tokenIntervals: List[Interval] = List.empty
) {

  def getOpt[T](list: List[T]): Option[T] = list.headOption
  def get[T](list: List[T]): T = list.head

  def documentOpt: Option[Document] = getOpt(documents)
  def document: Document = get(documents)

  def loopOpt: Option[Int] = getOpt(loops)
  def loop: Int = get(loops)

  def extractorOpt: Option[Extractor] = getOpt(extractors)
  def extractor: Extractor = get(extractors)

  def tokenPatternOpt: Option[TokenPattern] = getOpt(tokenPatterns)
  def tokenPattern: TokenPattern = get(tokenPatterns)

  def sentenceIndexOpt: Option[Int] = getOpt(sentenceIndexes)
  def sentenceIndex: Int = get(sentenceIndexes)

  // Is this really necessary?  Can the Sentences just come from the Document?
  // Probably for now, but we're not recording the order of entries into the context.
  def sentenceOpt: Option[Sentence] = getOpt(sentences)
  def sentence: Sentence = get(sentences)

  def startOpt: Option[Int] = getOpt(starts)
  def start: Int = get(starts)

  def tokOpt: Option[Int] = getOpt(toks)
  def tok: Int = get(toks)

  def tokenIntervalOpt: Option[Interval] = getOpt(tokenIntervals)
  def tokenInterval: Interval = get(tokenIntervals)
}
