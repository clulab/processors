package edu.arizona.sista.matcher

import scala.Equals
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

trait TextBoundMention extends Mention {
}

class TriggerMention(val label: String, val sentence: Int, val tokenInterval: Interval)
extends TextBoundMention

class EntityMention(val label: String, val sentence: Int, val tokenInterval: Interval)
extends TextBoundMention

class EventMention(val label: String, val sentence: Int, val arguments: Map[String, Mention])
extends Mention {
  override def tokenInterval: Interval = {
    val start = arguments.values.map(_.tokenFrom).min
    val end = arguments.values.map(_.tokenUntil).max
    Interval(start, end)
  }
}
