package org.clulab.processors.clu.bio

import java.util.regex.Pattern

import org.clulab.processors.clu.bio.BioLexicalVariations._
import org.clulab.sequences.LexicalVariations

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Generates all accepted lexical variations for this entity
  *   For example: "insulin receptor substrate 1" => "insulin receptor substrate-1"
  * User: mihais
  * Date: 10/20/16
  */
@SerialVersionUID(1000L)  
class BioLexicalVariations extends LexicalVariations {

  override def lexicalVariations(tokens: Array[String]): Seq[Array[String]] = {

    val variations = checkForVariations(tokens)

    if(variations.nonEmpty) {
      val newForms = new ListBuffer[Array[String]]
      if(variations.contains(MERGE_DASH_DIGIT)) {
        addMergeDashDigit(tokens, newForms)
      }
      if(variations.contains(SPLIT_DASH_DIGIT)) {
        addSplitDashDigit(tokens, newForms)
      }
      newForms
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

    // check for split dash digit; applies to only last token for now
    if(tokens.nonEmpty && ENDS_WITH_DASH_DIGITS.matcher(tokens.last).find()) {
      vars += SPLIT_DASH_DIGIT
    }

    vars.toSet
  }

  def addMergeDashDigit(tokens: Array[String], newForms:ListBuffer[Array[String]]) {
    var nts = new ArrayBuffer[String]()

    for(i <- 0 until tokens.length - 2) nts += tokens(i)
    nts += tokens(tokens.length - 2) + "-" + tokens(tokens.length - 1)

    newForms += nts.toArray
  }

  def addSplitDashDigit(tokens: Array[String], newForms:ListBuffer[Array[String]]): Unit = {
    var nts = new ArrayBuffer[String]()

    for(i <- 0 until tokens.length - 1) nts += tokens(i)

    val last = tokens.last
    val dashPos = last.lastIndexOf('-')
    assert(dashPos > 0 && dashPos < last.length - 1)
    nts += last.substring(0, dashPos)
    nts += last.substring(dashPos + 1)

    // println(s"ORIG: ${tokens.mkString(" ")} TO ${nts.mkString(" ")}")

    newForms += nts.toArray
  }
}

object BioLexicalVariations {
  val MERGE_DASH_DIGIT = 1
  val SPLIT_DASH_DIGIT = 2

  val IS_NUMBER: Pattern = Pattern.compile("[0-9]+")
  val ENDS_WITH_DASH_DIGITS: Pattern = Pattern.compile(".+\\-[0-9]+$")
}
