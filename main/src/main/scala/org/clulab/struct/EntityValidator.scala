package org.clulab.struct

import org.clulab.processors.Sentence
import org.clulab.sequences.LexiconNER

/**
  * Validates if the span identified as a named entity entity is actually valid
  * User: mihais
  * Date: 10/24/16
  */
trait EntityValidator {
  def validMatch(sentence: Sentence, start:Int, end:Int):Boolean
}

/**
  * Any span is considered as a valid entity
  * See org.clulab.processors.clu.bio.BioLexiconEntityValidator for a more complicated validator.
  */
class TrueEntityValidator extends EntityValidator {
  override def validMatch(sentence: Sentence, start:Int, end:Int): Boolean = true
}

object EntityValidator {
  val TRUE_VALIDATOR = new TrueEntityValidator
}