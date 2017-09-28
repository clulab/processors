package org.clulab.struct

import org.clulab.processors.Sentence
import org.clulab.sequences.LexiconNER

/**
  * Validates if the span identified as a named entity entity is actually valid
  * User: mihais
  * Date: 10/24/16
  */
trait EntityValidator {
  def validMatch(sentence:Sentence, start:Int, end:Int, lexNer:LexiconNER):Boolean
}

class TrueEntityValidator extends EntityValidator {
  def validMatch(sentence:Sentence, start:Int, end:Int, lexNer:LexiconNER):Boolean = true
}

object EntityValidator {
  val TRUE_VALIDATOR = new TrueEntityValidator
}