package org.clulab.struct

/**
  * Validates if the span identified as a named entity entity is actually valid
  * User: mihais
  * Date: 10/24/16
  */
trait EntityValidator {
  def validMatch(start:Int, end:Int):Boolean
}

class TrueEntityValidator extends EntityValidator {
  def validMatch(start:Int, end:Int):Boolean = true
}

object EntityValidator {
  val TRUE_VALIDATOR = new TrueEntityValidator
}