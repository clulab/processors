package org.clulab.processors.bionlp.ner

import java.util.regex.Pattern

import scala.collection.immutable.{HashSet, HashMap}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import LexicalVariations._

/**
  *
  * User: mihais
  * Date: 10/20/16
  */
class LexicalVariations {
  def lexicalVariations(tokens: Array[String]): Seq[Array[String]] = {

    val variations = checkForVariations(tokens)

    if(variations.nonEmpty) {
      val newForms = new ListBuffer[Array[String]]
      if(variations.contains(MERGE_DASH_DIGIT)) {
        addMergeDashDigit(tokens, newForms)
      }
      newForms.toSeq
    } else {
      Seq.empty
    }
  }

  /**
    * Read-only check to see if lexical variations are present
    *
    * @param tokens The original phrase
    * @return Map with types of variations set to true when present
    */
  def checkForVariations(tokens: Array[String]): Set[Int] = {
    val vars = new mutable.HashSet[Int]

    // check for merge dash digit; applies to only last token for now
    if(tokens.length > 1 && IS_NUMBER.matcher(tokens.last).matches()) {
      vars += MERGE_DASH_DIGIT
    }

    // TODO: check for split dash digit

    vars.toSet
  }

  def addMergeDashDigit(tokens: Array[String], newForms:ListBuffer[Array[String]]) {
    var nts = new ArrayBuffer[String]()

    for(i <- 0 until tokens.length - 2) nts += tokens(i)
    nts += tokens(tokens.length - 2) + "-" + tokens(tokens.length - 1)

    newForms += nts.toArray
  }
}

object LexicalVariations {
  val MERGE_DASH_DIGIT = 1
  val SPLIT_DASH_DIGIT = 2

  val IS_NUMBER = Pattern.compile("[0-9]+")
}
