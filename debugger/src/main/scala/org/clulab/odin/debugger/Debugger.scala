package org.clulab.odin.debugger

import org.clulab.odin.impl.{Extractor, Inst}
import org.clulab.processors.{Document, Sentence}

trait Debugger {
  def debugDoc[ResultType](document: Document)(block: => ResultType): ResultType = block

  def debugLoop[ResultType](loop: Int)(block: => ResultType): ResultType = block

  def debugSentence[ResultType](index: Int, sentence: Sentence)(block: => ResultType): ResultType = block

  def debugExtractor[ResultType](extractor: Extractor)(block: => ResultType): ResultType = block

  def debugStart[ResultType](start: Int)(block: => ResultType): ResultType = block

  def debugInst[ResultType](inst: Inst)(block: => ResultType): ResultType = block

  def debugMatches(matches: Boolean) = ()

  def debugTok[ResultType](tok: Int)(block: => ResultType): ResultType = block
}

class LocalDebugger extends Debugger {

}
