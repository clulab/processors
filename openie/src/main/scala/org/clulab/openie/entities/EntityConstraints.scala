package org.clulab.openie.entities

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.Mention


/**
  * Utilities for validating entities
  */
object EntityConstraints extends LazyLogging {

  val VALID_FINAL_TAG = """^(NN|VB|\-R[SR]B).*"""

  // POS tags for splitting conjunctions
  val coordPOS = Set(
    "CC",
    ","
  )

  /** Ensure final token of mention span is valid */
  def validFinalTag(mention: Mention): Boolean = mention.tags match {
    case None => true
    case Some(tags) => tags.last.matches(VALID_FINAL_TAG)
  }

  /** Limit entity mentions to at most n tokens */
  def withinMaxLength(mention: Mention, n: Int): Boolean = mention.words.size <= n

  /** Check if brackets and braces match */
  def matchingBrackets(mention: Mention): Boolean = {
    val pairs = Seq(("(", ")"), ("{", "}"), ("[", "]"))
    pairs.forall(pair => matchingBrackets(mention, pair._1, pair._2))
  }

  def matchingBrackets(mention: Mention, opening: String, closing: String): Boolean = {
    val lhsIdx = mention.words.indexOf(opening)
    val rhsIdx = mention.words.indexOf(closing)
    (lhsIdx, rhsIdx) match {
      // no brackets found
      case (-1, -1) => true
      // unmatched set
      case (-1, _) => false
      case (_, -1) => false
      // closing bracket appears before first opening
      case (broken1, broken2) if broken1 > broken2 => false
      // lhs precedes rhs, so count pairs
      case _ =>
        val lhsCnt = mention.words.count(_ == opening)
        val rhsCnt = mention.words.count(_ == closing)
        lhsCnt == rhsCnt
    }
  }

  /** Decide if the sentence element is a conjunction using just the POS tag **/
  def isCoord(i: Int, m: Mention): Boolean = coordPOS.contains(m.sentenceObj.tags.get(i))

}