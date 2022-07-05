package org.clulab.openie.entities

import org.clulab.odin.Mention
import org.clulab.openie.utils.TagSet
import org.clulab.utils.Logging

import scala.annotation.tailrec


/**
  * Utilities for validating entities
  */
object EntityConstraints extends Logging {
  val pairs = Seq(("(", ")"), ("{", "}"), ("[", "]"))

  // POS tags for splitting conjunctions
  val coordPOS = Set(
    "CC",
    ","
  )

  /** Ensure final token of mention span is valid */
  def validFinalTag(mention: Mention, tagSet: TagSet): Boolean =
    mention.tags.isEmpty || tagSet.isValidFinal(mention.tags.get.last)


  /** Limit entity mentions to at most n tokens */
  def withinMaxLength(mention: Mention, n: Int): Boolean = mention.words.size <= n

  /** Check if brackets and braces match */
  def matchingBrackets(mention: Mention): Boolean =
      matchingBrackets(mention.words)

  def matchingBrackets(words: Seq[String]): Boolean =
      pairs.forall(pair => matchingBrackets(words, pair._1, pair._2))

  def matchingBrackets(words: Seq[String], opening: String, closing: String): Boolean = {

    @tailrec
    def loop(index: Int, extraOpening: Int): Boolean = {
      if (extraOpening < 0)
        false // too many closing without opening
      else if (index >= words.length)
        extraOpening == 0 // if it is just right
      else if (words(index) == opening)
        loop(index + 1, extraOpening + 1)
      else if (words(index) == closing)
        loop(index + 1, extraOpening - 1)
      else
        loop(index + 1, extraOpening)
    }

    loop(0, 0)
  }

  /** Decide if the sentence element is a conjunction using just the POS tag **/
  def isCoord(i: Int, mention: Mention, tagSet: TagSet): Boolean = {
    if (i > 0 && tagSet.isAnyAdjective(mention.sentenceObj.tags.get(i - 1))) false
    else tagSet.isCoordinating(mention.sentenceObj.tags.get(i))
  }

}