package org.clulab.ie.entities

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.Mention

/**
  * Utilities for validating entities
  */
object EntityConstraints extends LazyLogging {

  val eventTriggers = """(?i)activ|regul|acceler|activat|aid|augment|cataly|caus|driv|elev|elicit|enabl|enhanc|exacerbat|increas|induc|initi|interconvert|lead|led|overexpress|potenti|produc|prolong|promot|rais|reactivat|re-express|releas|rescu|restor|signal|stimul|synerg|synthes|target|trigger|up-regul|upregul|abolish|abrog|antagon|attenu|block|deactiv|decreas|degrad|deplet|deregul|diminish|disengag|disrupt|down-reg|downreg|dysregul|elimin|impair|imped|inactiv|inhibit|knockdown|limit|loss|lower|negat|neutraliz|nullifi|perturb|prevent|reduc|reliev|remov|repress|resist|restrict|revers|sequester|shutdown|slow|starv|supp?ress|uncoupl""".r

  // conservative subset of terms denoting coref
  val corefTerms = """(?i)^(such)$""".r //"""(?i)^(this|such)$""".r

  val XREF = """XREF_""".r

  val VALID_FINAL_TAG = """^(NN|VB|\-R[SR]B).*"""

  // POS tags for splitting conjunctions
  val coordPOS = Set(
    "CC",
    ","
  )

  /** Checks ito see if any tokens in the (expanded) Mention's span appear to be XREFs */
  def containsXref(mention: Mention): Boolean = mention.words.exists(containsXref)
  def containsXref(w: String): Boolean = XREF.findFirstMatchIn(w).nonEmpty

  // NOTE: this is no longer used.  Now we filter this at the event level using InfluenceActions.disallowTriggerOverlap
  /** Checks to see if any tokens in the (expanded) Mention's span appear to be an event trigger */
  def containsEventTrigger(mention: Mention): Boolean = mention.words.exists(containsEventTrigger)
  def containsEventTrigger(w: String): Boolean = eventTriggers.findFirstMatchIn(w).nonEmpty

  def involvesCoreference(mention: Mention): Boolean = mention.words.exists(involvesCoreference)
  def involvesCoreference(w: String): Boolean = corefTerms.findFirstMatchIn(w).nonEmpty

  /** Ensure final token of mention span is valid */
  def validFinalTag(mention: Mention): Boolean = mention.tags match {
    case None => true
    case Some(tags) => tags.last.matches(VALID_FINAL_TAG)
  }

  /** Limit entity mentions to at most n tokens */
  def withinMaxLength(mention: Mention, n: Int = 10): Boolean = mention.words.size <= n

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