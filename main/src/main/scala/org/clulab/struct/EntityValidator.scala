package org.clulab.struct

import org.clulab.processors.Sentence
import org.clulab.sequences.LexiconNER

/**
  * Validates if the span identified as a named entity entity is actually valid
  * User: mihais
  * Date: 10/24/16
  */
@SerialVersionUID(1000L)
trait EntityValidator extends Serializable {
  def validMatch(sentence: Sentence, start:Int, end:Int):Boolean
}

/**
  * Any span is considered as a valid entity
  * See org.clulab.processors.bio.BioLexiconEntityValidator for a more complicated validator.
  */
@SerialVersionUID(1000L)  
class TrueEntityValidator extends EntityValidator {
  override def validMatch(sentence: Sentence, start:Int, end:Int): Boolean = true
}

object EntityValidator {
  val TRUE_VALIDATOR = new TrueEntityValidator
}