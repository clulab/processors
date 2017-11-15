package org.clulab.ie

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.utils.DependencyUtilsException


class InfluenceActions extends Actions with LazyLogging {

  // include patterns defined in the companion object
  import InfluenceActions._

  val Transparent: Set[String] = VocabularyConstraints.Transparent

  /**
    * Ensures no Event mention has arguments that overlap
    */
  def removeOverlappingEvents(mentions: Seq[Mention], state: State): Seq[Mention] = mentions filter {
    // ensure no event has an overlapping cause/effect
    case em if em matches influenceEvent =>
      val causes = em.arguments.getOrElse(causeRole, Nil)
      val effects = em.arguments.getOrElse(effectRole, Nil)
      causes.forall{ cause =>
        effects.forall{ effect =>
          ! cause.tokenInterval.overlaps(effect.tokenInterval)
        }
      }
    // keep anything else
    case _ => true
  }

  def analyzePolarity(mentions: Seq[Mention], state: State): Seq[Mention] = mentions.map(SemanticPolarity.analyzeSemanticPolarity)

}


object InfluenceActions extends LazyLogging {

  val causeRole = "controller"
  val effectRole = "controlled"
  val influenceEvent = "CausalEvent"
  val increaseEvent = "IncreaseEvent"
  val decreaseEvent = "DecreaseEvent"

  val INCREASE = "increases"
  val DECREASE = "decreases"

  // org.apache.lucene.analysis.StopAnalyzer.ENGLISH_STOP_WORDS_SET
  val stopWords = Set(
    "a", "an", "and", "are", "as", "at", "be", "but", "by",
    "for", "if", "in", "into", "is", "it",
    "no", "not", "of", "on", "or", "such",
    "that", "the", "their", "then", "there", "these",
    "they", "this", "to", "was", "will", "with"
  )

  val MODIFIER_LABELS = "amod".r

  // these patterns are meant to be applied on the lemmas
  // NOTE: these should match whatever is in rules.yml
  val posRegTriggerPattern = "acceler|activat|aid|augment|cataly|caus|contribut|driv|elev|elicit|enabl|enhanc|exacerbat|increas|induc|initi|interconvert|lead|led|overexpress|potenti|produc|prolong|promot|rais|reactivat|re-express|releas|rescu|restor|signal|stimul|support|synerg|synthes|target|trigger|underli|up-regul|upregul".r

  val negRegTriggerPattern = "abolish|abrog|antagon|attenu|block|deactiv|decreas|degrad|deplet|deregul|diminish|disengag|disrupt|down-reg|downreg|dysregul|elimin|impair|imped|inactiv|inhibit|knockdown|limit|loss|lower|negat|neutraliz|nullifi|perturb|prevent|reduc|reliev|remov|repress|resist|restrict|revers|sequester|shutdown|slow|starv|supp?ress|uncoupl".r


  def matchesRegulationTrigger(s: String): Boolean = {
    posRegTriggerPattern.findFirstIn(s).nonEmpty || negRegTriggerPattern.findFirstIn(s).nonEmpty
  }

  def getSemHeadLemma(m: Mention): Option[String] = {
    try {
      m.semHeadLemma
    } catch {
      case e: DependencyUtilsException =>
        logger.info(s"Error finding '.semHeadLemma' for sentence: '${m.sentenceObj.getSentenceText}'")
        None
    }
  }

  def getSemHeadTag(m: Mention): Option[String] = {
    try {
      m.semHeadTag
    } catch {
      case e: DependencyUtilsException =>
        logger.info(s"Error finding '.semHeadTag' for sentence: '${m.sentenceObj.getSentenceText}'")
        None
    }
  }

  /**
    * Don't allow event arguments to overlap with the event's trigger
    */
  def disallowTriggerOverlap(mentions: Seq[Mention], state: State): Seq[Mention] = mentions filter {

    case em: EventMention if em matches influenceEvent =>
      em.arguments.values.flatten.forall( arg => ! arg.tokenInterval.overlaps(em.trigger.tokenInterval))

    case _ => true

  }

}