package org.clulab.odin.debugger.debug

import org.clulab.odin.Mention
import org.clulab.odin.impl.ThompsonVM.SingleThread
import org.clulab.odin.impl.{Done, Extractor, Inst, TokenPattern}
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.Interval

class DebuggerContext(
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
  protected var insts: List[Inst] = List.empty, // TODO: skip toks and insts because of Threads
  protected var tokenIntervals: List[Interval] = List.empty
) {
  def isComplete: Boolean = {
    documents.nonEmpty &&
      loops.nonEmpty &&
      extractors.nonEmpty &&
      tokenPatterns.nonEmpty &&
      sentenceIndexes.nonEmpty &&
      sentences.nonEmpty

    // tokenIntervals
  }

  def isInstComplete: Boolean = {
    isComplete && {
      starts.nonEmpty &&
        toks.nonEmpty //&&
      //      insts.nonEmpty
    }
  }

  def isThreadComplete: Boolean = {
    isComplete && {
      starts.nonEmpty
    }
  }

  def getDepth: Int = depth

  def setDocument(document: Document): Unit = {
    documents = document :: documents
    depth += 1
  }

  def getDocumentOpt: Option[Document] = documents.headOption

  def resetDocument(): Unit = {
    documents = documents.tail
    depth -= 1
  }

  def setLoop(loop: Int): Unit = {
    loops = loop :: loops
    depth += 1
  }

  def getLoopOpt: Option[Int] = loops.headOption

  def resetLoop(): Unit = {
    loops = loops.tail
    depth -= 1
  }

  def setExtractor(extractor: Extractor): Unit = {
    extractors = extractor :: extractors
    depth += 1
  }

  def getExtractorOpt: Option[Extractor] = extractors.headOption

  def resetExtractor(): Unit = {
    extractors = extractors.tail
    depth -= 1
  }

  def setTokenPattern(tokenPattern: TokenPattern): Unit = {
    tokenPatterns = tokenPattern :: tokenPatterns
    depth += 1
  }

  def getTokenPattern: Option[TokenPattern] = tokenPatterns.headOption

  def resetTokenPattern(): Unit = {
    tokenPatterns = tokenPatterns.tail
    depth -= 1
  }

  def setSentence(sentenceIndex: Int, sentence: Sentence): Unit = {
    sentenceIndexes = sentenceIndex :: sentenceIndexes
    sentences = sentence :: sentences
    depth += 1
  }

  def getSentenceOpt: (Option[Int], Option[Sentence]) = (sentenceIndexes.headOption, sentences.headOption)

  def resetSentence(): Unit = {
    sentenceIndexes = sentenceIndexes.tail
    sentences = sentences.tail
    depth -= 1
  }

  def setStart(start: Int): Unit = {
    starts = start :: starts
    depth += 1
  }

  def getStartOpt: Option[Int] = starts.headOption

  def resetStart(): Unit = {
    starts = starts.tail
    depth -= 1
  }

  def setTok(tok: Int): Unit = {
    toks = tok :: toks
    depth += 1
  }

  def getTokOpt: (Option[Int]) = toks.headOption

  def getToksLength: Int = toks.length

  def resetTok(): Unit = {
    toks = toks.tail
    depth -= 1
  }

  def setTokenInterval(tokenInterval: Interval): Unit = {
    tokenIntervals = tokenInterval :: tokenIntervals
    depth += 1
  }

  def getTokenIntervalOpt: (Option[Interval]) = tokenIntervals.headOption

  def resetTokenInterval(): Unit = {
    tokenIntervals = tokenIntervals.tail
    depth -= 1
  }


  def setInst(inst: Inst): Unit = {
    insts = inst :: insts
    depth += 1
  }

  def getInstOpt: Option[Inst] = insts.headOption

  def getInstsLength: Int = insts.length

  def resetInst(): Unit = {
    insts = insts.tail
    depth -= 1
  }

  def mkDebuggerRecord(tok: Int): DebuggerRecord = {
    DebuggerRecord(
      documents.head,
      loops.head,
      extractors.head,
      tokenPatterns.head,
      sentenceIndexes.head,
      sentences.head,

      starts.headOption,
      Some(tok) // ,
      //      Some(inst),
      //      tokenIntervals.headOption,
      //      matches
    )
  }

  def setInstMatches(matches: Boolean, tok: Int, inst: Inst): FinishedInst = {

    if (!isInstComplete) {
      if (inst != Done)
        println("The record is not complete!")

    } // TODO: Depends on what kind of extractor.  How does one know?  Check most recent extractor?
    // Leave something else on the stack.


    if (tokenIntervals.nonEmpty) {
      // This is for cross
      val asExpected = starts.isEmpty && toks.isEmpty && insts.isEmpty
    }
    else {
      val asExpected = starts.nonEmpty && toks.nonEmpty && insts.nonEmpty
    }

    FinishedInst(inst, matches, mkDebuggerRecord(tok))
  }

  def setThreadMatches(thread: SingleThread, instMatches: Boolean, threadMatch: ThreadMatch): FinishedThread = {
    val debuggerRecord = mkDebuggerRecord(thread.tok)
    val finishedThread = FinishedThread(thread, instMatches, threadMatch, debuggerRecord)

    if (!isComplete)
      println("The record is not complete!")// TODO: Depends on what kind of extractor

    if (tokenIntervals.nonEmpty) {
      // This is for cross
      val asExpected = starts.isEmpty && toks.isEmpty && insts.isEmpty
    }
    else {
      val asExpected = starts.nonEmpty && toks.nonEmpty && insts.nonEmpty
    }

    finishedThread
  }

  def setLocalAction(inMentions: Seq[Mention], outMentions: Seq[Mention]): FinishedLocalAction = {
    require(extractors.nonEmpty) // TODO: And other things

    // Check to see whether still within an Extractor
    val debuggerRecord = DebuggerRecordForLocalAction(
      documents.head,
      loops.head,
      extractors.head,
      // tokenPatterns.head, // Not for graph extractor
      sentenceIndexes.head,
      sentences.head
    )
    val finishedAction = FinishedLocalAction(inMentions, outMentions, debuggerRecord)

    finishedAction
  }

  def setGlobalAction(inMentions: Seq[Mention], outMentions: Seq[Mention]): FinishedGlobalAction = {
    require(extractors.isEmpty)

    val debuggerRecord = DebuggerRecordForGlobalAction(
      documents.head,
      loops.head
    )
    val finishedAction = FinishedGlobalAction(inMentions, outMentions, debuggerRecord)

    finishedAction
  }
}
