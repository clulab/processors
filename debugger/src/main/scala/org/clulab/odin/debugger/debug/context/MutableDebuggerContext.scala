package org.clulab.odin.debugger.debug.context

import org.clulab.odin.Mention
import org.clulab.odin.debugger.debug.filter.DynamicDebuggerFilter
import org.clulab.odin.debugger.debug.finished._
import org.clulab.odin.debugger.debug.matches.{MentionMatch, ThreadMatch}
import org.clulab.odin.impl.ThompsonVM.SingleThread
import org.clulab.odin.impl.{Extractor, Inst, TokenPattern}
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Interval

class MutableDebuggerContext(filter: DynamicDebuggerFilter) {
  protected var depth: Int = 0
  // TODO: Add ExtractorEngine to this, because there may be multiple.
  protected var documents: List[Document] = List.empty
  protected var loops: List[Int] = List.empty
  protected var extractors: List[Extractor] = List.empty
  protected var tokenPatterns: List[TokenPattern] = List.empty
  protected var sentenceIndexes: List[Int] = List.empty
  protected var sentences: List[Sentence] = List.empty
  protected var starts: List[Int] = List.empty // Where in the sentence we are starting.
  protected var toks: List[Int] = List.empty // The current token index
  protected var tokenIntervals: List[Interval] = List.empty

  def getDepth: Int = depth - 1

  def setValue[T](list: List[T], value: T): List[T] = {
    depth += 1
    value :: list
  }

  def getValueOpt[T](list: List[T]): Option[T] = list.headOption

  def resetValue[T](list: List[T]): List[T] = {
    depth -= 1
    list.tail
  }

  def newStaticDebuggerContext(): ImmutableDebuggerContext = ImmutableDebuggerContext(
    depth,
    documents,
    loops,
    extractors,
    tokenPatterns,
    sentenceIndexes,
    sentences,
    starts,
    toks,
    tokenIntervals
  )

  def beforeDuringAfter[ResultType](before: => Unit, during: => ResultType, after: => Unit): ResultType = {
    before
    try {
      during
    }
    finally {
      after
    }
  }

  def setDocument(document: Document): Unit = documents = setValue(documents, document)
  def getDocumentOpt: Option[Document] = getValueOpt(documents)
  def resetDocument(): Unit = documents = resetValue(documents)
  def withDocument[ResultType](document: Document)(block: => ResultType): ResultType = {
    // Note that we can only filter at the very end.  At this stage, neither the
    // Extractor nor Sentence is defined in the context, so the context recordkeeping
    // would not be performed.
    beforeDuringAfter(setDocument(document), block, resetDocument())
  }

  def setLoop(loop: Int): Unit = loops = setValue(loops, loop)
  def getLoopOpt: Option[Int] = getValueOpt(loops)
  def resetLoop(): Unit = loops = resetValue(loops)
  def withLoop[ResultType](loop: Int)(block: => ResultType): ResultType = {
    beforeDuringAfter(setLoop(loop), block, resetLoop())
  }

  def setExtractor(extractor: Extractor): Unit = extractors = setValue(extractors, extractor)
  def getExtractorOpt: Option[Extractor] = getValueOpt(extractors)
  def resetExtractor(): Unit = extractors = resetValue(extractors)
  def withExtractor[ResultType](extractor: Extractor)(block: => ResultType): ResultType = {
    beforeDuringAfter(setExtractor(extractor), block, resetExtractor())
  }

  def setTokenPattern(tokenPattern: TokenPattern): Unit = tokenPatterns = setValue(tokenPatterns, tokenPattern)
  def getTokenPattern: Option[TokenPattern] = getValueOpt(tokenPatterns)
  def resetTokenPattern(): Unit = tokenPatterns = resetValue(tokenPatterns)
  def withTokenPattern[ResultType](tokenPattern: TokenPattern)(block: => ResultType): ResultType = {
    beforeDuringAfter(setTokenPattern(tokenPattern), block, resetTokenPattern())
  }

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
  def withSentence[ResultType](sentenceIndex: Int, sentence: Sentence)(block: => ResultType): ResultType = {
    beforeDuringAfter(setSentence(sentenceIndex, sentence), block, resetSentence())
  }

  def setStart(start: Int): Unit = starts = setValue(starts, start)
  def getStartOpt: Option[Int] = getValueOpt(starts)
  def resetStart(): Unit = starts = resetValue(starts)
  def withStart[ResultType](start: Int)(block: => ResultType): ResultType = {
    beforeDuringAfter(setStart(start), block, resetStart())
  }

  def setTok(tok: Int): Unit = toks = setValue(toks, tok)
  def getTokOpt: (Option[Int]) = getValueOpt(toks)
  def resetTok(): Unit = toks = resetValue(toks)
  def withTok[ResultType](tok: Int)(block: => ResultType): ResultType = {
    beforeDuringAfter(setTok(tok), block, resetTok())
  }

  def setTokenInterval(tokenInterval: Interval): Unit = tokenIntervals = setValue(tokenIntervals, tokenInterval)
  def getTokenIntervalOpt: (Option[Interval]) = getValueOpt(tokenIntervals)
  def resetTokenInterval(): Unit = tokenIntervals = resetValue(tokenIntervals)
  def withTokenInterval[ResultType](tokenInterval: Interval)(block: => ResultType): ResultType = {
    beforeDuringAfter(setTokenInterval(tokenInterval), block, resetTokenInterval())
  }

  def setInstMatches(matches: Boolean, tok: Int, inst: Inst): Option[FinishedInst] = {
    beforeDuringAfter(
      setTok(tok),
      {
        val staticDebuggerContext = newStaticDebuggerContext()

        // Here the filter is used, because it decides whether Finisheds are added to the Transcripts.
        if (filter(staticDebuggerContext))
          Some(new FinishedInst(staticDebuggerContext, inst, matches))
        else None
      },
      resetTok()
    )
  }

  def setThreadMatches(thread: SingleThread, instMatch: Boolean, threadMatch: ThreadMatch): Option[FinishedThread] = {
    val staticDebuggerContext = newStaticDebuggerContext()

    if (filter(staticDebuggerContext))
      Some(new FinishedThread(newStaticDebuggerContext(), thread, instMatch, threadMatch))
    else None
  }

  def setMentionMatches(mention: Mention, stateMentions: Seq[Mention], mentionMatches: Seq[MentionMatch]): Option[FinishedMention] = {
    val staticDebuggerContext = newStaticDebuggerContext()

    if (filter(staticDebuggerContext))
      Some(new FinishedMention(newStaticDebuggerContext(), mention, stateMentions, mentionMatches))
    else None
  }

  def setLocalActionMatches(inMentions: Seq[Mention], outMentions: Seq[Mention]): Option[FinishedLocalAction] = {
    val staticDebuggerContext = newStaticDebuggerContext()

    if (filter(staticDebuggerContext))
      Some(new FinishedLocalAction(newStaticDebuggerContext(), inMentions, outMentions))
    else None
  }

  def setGlobalActionMatches(inMentions: Seq[Mention], outMentions: Seq[Mention]): Option[FinishedGlobalAction] = {
    val staticDebuggerContext = newStaticDebuggerContext()

    // Do not filter the global actions unless we are very careful about the filter.
    // It has only the document and the loop.
//    if (filter(staticDebuggerContext))
      Some(new FinishedGlobalAction(newStaticDebuggerContext(), inMentions, outMentions))
//    else None
  }
}

