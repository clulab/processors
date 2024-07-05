package org.clulab.processors.clu.tokenizer

import scala.collection.mutable.ArrayBuffer

/**
  * Resolves Spanish contractions
  * Author: dane
  * Author: mihais
  * Date: 7/23/2018
  */
class TokenizerStepSpanishContractions extends TokenizerStep {
  // is this word ALL CAPS, Sentence Caps, or lowercase?
  protected def cap(s: String): String = {
    val letters = s.filter(_.isLetter)
    letters match {
      case "" => "lower"
      case lower if !letters.head.isUpper => "lower"
      case upper if letters.length > 1 && letters.forall(c => !c.isLower) => "upper"
      case sentenceCaps if !letters.head.isLower => "sentence"
      case _ => "lower"
    }
  }

  //
  protected def matchCase(source: String, target: String): String = cap(source) match {
    case "lower" => target.toLowerCase
    case "sentence" => target.head.toUpper +: target.tail
    case "upper" => target.toUpperCase
  }

  override def process(inputs:Array[RawToken]): Array[RawToken] = {
    //
    // We must separate important linguistic constructs here
    // TODO: this is slow. This should be handled in the Antlr grammar
    //

    val tokens = new ArrayBuffer[RawToken]()

    for(input <- inputs) {

      if("""(?i)^al$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "a"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "el")
      }
      else if("""(?i)^del$""".r.findFirstIn(input.raw).isDefined) {
        tokens += RawToken(input.raw.substring(0, 1), input.beginPosition, matchCase(input.raw, "de"))
        tokens += RawToken(input.raw.substring(1), input.beginPosition, "el")
      }
      // any other token
      else {
        tokens += input
      }
    }

    tokens.toArray
  }
}

