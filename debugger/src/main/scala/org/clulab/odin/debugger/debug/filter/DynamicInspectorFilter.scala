package org.clulab.odin.debugger.debug.filter

import org.clulab.odin.debugger.debug.Transcript
import org.clulab.odin.debugger.debug.finished.{FinishedGlobalAction, FinishedInst, FinishedLocalAction, FinishedMention, FinishedThread}
import org.clulab.odin.impl.Extractor
import org.clulab.processors.Sentence

class DynamicInspectorFilter extends InspectorFilter {
  def showParseView(sentence: Sentence): Boolean = true

  def showRuleView(extractor: Extractor, sentence: Sentence): Boolean = true
  def showTextualView(extractor: Extractor, sentence: Sentence): Boolean = true
  def showGraphicalView(extractor: Extractor, sentence: Sentence): Boolean = true

  def showInstView(extractor: Extractor, sentence: Sentence, transcript: Transcript[FinishedInst]): Boolean = true
  def showThreadView(extractor: Extractor, sentence: Sentence, transcript: Transcript[FinishedThread]): Boolean = true
  def showMentionView(extractor: Extractor, sentence: Sentence, transcript: Transcript[FinishedMention]): Boolean = true
  def showLocalActionView(extractor: Extractor, sentence: Sentence, transcript: Transcript[FinishedLocalAction]): Boolean = true
  def showGlobalActionView(transcript: Transcript[FinishedGlobalAction]): Boolean = true
}

object DynamicInspectorFilter {
  val verbose = new DynamicInspectorFilter()
  val concise = new DynamicInspectorFilter {
    override def showRuleView(extractor: Extractor, sentence: Sentence): Boolean = false
    override def showTextualView(extractor: Extractor, sentence: Sentence): Boolean = false
    override def showGraphicalView(extractor: Extractor, sentence: Sentence): Boolean = false
  }
}