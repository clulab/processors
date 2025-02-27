package org.clulab.odin.debugger.debug

import org.clulab.odin.Mention
import org.clulab.odin.debugger.debug.finished.{FinishedGlobalAction, FinishedInst, FinishedLocalAction, FinishedMention, FinishedThread}
import org.clulab.odin.impl.ThompsonVM.SingleThread
import org.clulab.odin.impl.{Extractor, Inst, TokenPattern}
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Interval

class DynamicDebuggerContext(
  protected var depth: Int = 0,
  // TODO: Add ExtractorEngine to this, because there may be multiple.
  protected var documents: List[Document] = List.empty,
  protected var loops: List[Int] = List.empty,
  protected var extractors: List[Extractor] = List.empty,
  protected var tokenPatterns: List[TokenPattern] = List.empty,
  protected var sentenceIndexes: List[Int] = List.empty,
  protected var sentences: List[Sentence] = List.empty,
  protected var starts: List[Int] = List.empty, // Where in the sentence we are starting.
  protected var toks: List[Int] = List.empty, // The current token index
  protected var insts: List[Inst] = List.empty, // TODO: Remove
  protected var tokenIntervals: List[Interval] = List.empty
) {
  def getDepth: Int = depth

  def setValue[T](list: List[T], value: T): List[T] = {
    depth += 1
    value :: list
  }

  def getValueOpt[T](list: List[T]): Option[T] = list.headOption

  def resetValue[T](list: List[T]): List[T] = {
    depth -= 1
    list.tail
  }

  def newStaticDebuggerContext(tok: Int): StaticDebuggerContext = {
    newStaticDebuggerContext(setValue(toks, tok))
  }

  def newStaticDebuggerContext(): StaticDebuggerContext = {
    newStaticDebuggerContext(toks)
  }

  def newStaticDebuggerContext(toks: List[Int]): StaticDebuggerContext = StaticDebuggerContext(
    depth,
    documents,
    loops,
    extractors,
    tokenPatterns,
    sentenceIndexes,
    sentences,
    starts,
    toks,
    insts,
    tokenIntervals
  )

  def setDocument(document: Document): Unit = documents = setValue(documents, document)
  def getDocumentOpt: Option[Document] = getValueOpt(documents)
  def resetDocument(): Unit = documents = resetValue(documents)

  def setLoop(loop: Int): Unit = loops = setValue(loops, loop)
  def getLoopOpt: Option[Int] = getValueOpt(loops)
  def resetLoop(): Unit = loops = resetValue(loops)

  def setExtractor(extractor: Extractor): Unit = extractors = setValue(extractors, extractor)
  def getExtractorOpt: Option[Extractor] = getValueOpt(extractors)
  def resetExtractor(): Unit = extractors = resetValue(extractors)

  def setTokenPattern(tokenPattern: TokenPattern): Unit = tokenPatterns = setValue(tokenPatterns, tokenPattern)
  def getTokenPattern: Option[TokenPattern] = getValueOpt(tokenPatterns)
  def resetTokenPattern(): Unit = tokenPatterns = resetValue(tokenPatterns)

  def setSentence(sentenceIndex: Int, sentence: Sentence): Unit = {
    sentenceIndexes = setValue(sentenceIndexes, sentenceIndex)
    sentences = setValue(sentences, sentence)
    depth -= 1 // It will be counted as just one level of depth.
  }
  def getSentenceOpt: (Option[Int], Option[Sentence]) = (getValueOpt(sentenceIndexes), getValueOpt(sentences))
  def resetSentence(): Unit = {
    depth += 1 // It will be counted as just one level of depth.
    sentences = resetValue(sentences)
    sentenceIndexes = resetValue(sentenceIndexes)
  }

  def setStart(start: Int): Unit = starts = setValue(starts, start)
  def getStartOpt: Option[Int] = getValueOpt(starts)
  def resetStart(): Unit = starts = resetValue(starts)

  def setTok(tok: Int): Unit = toks = setValue(toks, tok)
  def getTokOpt: (Option[Int]) = getValueOpt(toks)
  def resetTok(): Unit = toks = resetValue(toks)

  def setTokenInterval(tokenInterval: Interval): Unit = tokenIntervals = setValue(tokenIntervals, tokenInterval)
  def getTokenIntervalOpt: (Option[Interval]) = getValueOpt(tokenIntervals)
  def resetTokenInterval(): Unit = tokenIntervals = resetValue(tokenIntervals)

  def setInst(inst: Inst): Unit = insts = setValue(insts, inst)
  def getInstOpt: Option[Inst] = getValueOpt(insts)
  def resetInst(): Unit = insts = resetValue(insts)

  def setInstMatches(matches: Boolean, tok: Int, inst: Inst): FinishedInst = {
    new FinishedInst(newStaticDebuggerContext(tok), inst, matches)
  }

  def setThreadMatches(thread: SingleThread, threadMatch: ThreadMatch): FinishedThread = {
    new FinishedThread(newStaticDebuggerContext(), thread, threadMatch)
  }

  def setMentionMatches(mention: Mention, stateMentions: Seq[Mention], mentionMatches: Seq[MentionMatch]): FinishedMention = {
    new FinishedMention(newStaticDebuggerContext(), mention, stateMentions, mentionMatches)
  }

  def setLocalAction(inMentions: Seq[Mention], outMentions: Seq[Mention]): FinishedLocalAction = {
    new FinishedLocalAction(newStaticDebuggerContext(), inMentions, outMentions)
  }

  def setGlobalAction(inMentions: Seq[Mention], outMentions: Seq[Mention]): FinishedGlobalAction = {
    new FinishedGlobalAction(newStaticDebuggerContext(), inMentions, outMentions)
  }
}

