package edu.arizona.sista.matcher

import scala.util.hashing.MurmurHash3._
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.processors.Document

trait Mention extends Equals {
  def label: String
  def tokenInterval: Interval
  def sentence: Int
  def document: Document

  // name of matching rule
  def foundBy: String

  def start: Int = tokenInterval.start
  def end: Int = tokenInterval.end

  // this method should be overriden by Mention subclases
  // to return the label and all the relevant labels in some taxonomy
  def allLabels: Set[String] = Set(label)

  // a mention matches a label if the label is returned by allLabels
  def matches(label: String): Boolean = allLabels contains label

  override def canEqual(a: Any) = a.isInstanceOf[Mention]

  override def equals(that: Any): Boolean = that match {
    case that: Mention => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def hashCode: Int = {
    val h0 = symmetricSeed
    val h1 = mix(h0, label.hashCode)
    val h2 = mix(h1, tokenInterval.hashCode)
    val h3 = mix(h2, sentence.hashCode)
    val h4 = mixLast(h3, document.hashCode)
    finalizeHash(h4, 4)
  }
}

class TextBoundMention(val label: String,
                       val tokenInterval: Interval,
                       val sentence: Int,
                       val document: Document,
                       val foundBy: String) extends Mention

class EventMention(val label: String,
                   val trigger: TextBoundMention,
                   val arguments: Map[String, Seq[Mention]],
                   val sentence: Int,
                   val document: Document,
                   val foundBy: String) extends Mention {
  // token interval that contains trigger and all matched arguments
  override def tokenInterval: Interval = {
    val allStarts = trigger.start +: arguments.values.flatMap(_.map(_.start)).toSeq
    val allEnds = trigger.end +: arguments.values.flatMap(_.map(_.end)).toSeq
    Interval(allStarts.min, allEnds.max)
  }
}
