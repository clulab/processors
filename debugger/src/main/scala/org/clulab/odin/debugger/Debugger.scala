package org.clulab.odin.debugger

import org.clulab.odin.impl.{Extractor, Inst}
import org.clulab.processors.{Document, Sentence}

trait Debugger {
  def debugDoc[ResultType](document: Document)(block: => ResultType): ResultType = ???

  def debugLoop[ResultType](loop: Int)(block: => ResultType): ResultType = ???

  def debugSentence[ResultType](index: Int, sentence: Sentence)(block: => ResultType): ResultType = ???

  def debugExtractor[ResultType](extractor: Extractor)(block: => ResultType): ResultType = ???

  def debugStart[ResultType](start: Int)(block: => ResultType): ResultType = ???

  def debugInst[ResultType](inst: Inst)(block: => ResultType): ResultType = ???

  def debugMatches(matches: Boolean) = ???

  def debugTok[ResultType](tok: Int)(block: => ResultType): ResultType = ???
}

class LocalDebugger extends Debugger {

}
