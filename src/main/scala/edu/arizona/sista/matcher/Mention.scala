package edu.arizona.sista.matcher

import scala.util.hashing.MurmurHash3._
import edu.arizona.sista.struct.Interval

trait Mention extends Equals {
  def label: String
  def sentence: Int
  def tokenInterval: Interval

  // name of matching rule
  var foundBy: Option[String] = None

  def tokenFrom: Int = tokenInterval.start
  def tokenUntil: Int = tokenInterval.end

  // this method should be overriden by Mention subclases
  // to return the label and all the relevant labels in some taxonomy
  def allLabels: Seq[String] = Seq(label)

  def matchesLabel(label: String): Boolean = allLabels exists (_ == label)

  override def canEqual(a: Any) = a.isInstanceOf[Mention]

  override def equals(that: Any): Boolean = that match {
    case that: Mention => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def hashCode: Int = {
    val h = symmetricSeed
    val h1 = mix(h, label.hashCode)
    val h2 = mix(h1, sentence.hashCode)
    val h3 = mixLast(h2, tokenInterval.hashCode)
    finalizeHash(h3, 3)
  }
}

trait TextBoundMention extends Mention

class TriggerMention(val label: String, val sentence: Int, val tokenInterval: Interval)
extends TextBoundMention

class EntityMention(val label: String, val sentence: Int, val tokenInterval: Interval)
extends TextBoundMention

class EventMention(val label: String, val trigger: TextBoundMention, val arguments: Map[String, Seq[Mention]])
extends Mention {
  override def sentence: Int = trigger.sentence

  // token interval that contains trigger and all matched arguments
  override def tokenInterval: Interval = {
    val allStarts = arguments.values.flatMap(_.map(_.tokenFrom)).toSeq :+ trigger.tokenFrom
    val allEnds = arguments.values.flatMap(_.map(_.tokenUntil)).toSeq :+ trigger.tokenUntil
    Interval(allStarts.min, allEnds.max)
  }
}

class EquivMention(val mentions: Set[Mention]) extends Mention {
  override val label = "Equiv"

  override val sentence = mentions.map(_.sentence).min

  // this feature doesn't really make sense for EquivMentions
  // maybe add a way to sort mentions by sentence and tokens in Mention trait
  override def tokenInterval: Interval = mentions.minBy(_.sentence).tokenInterval
}
